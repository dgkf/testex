#' Helpers for using testex with testthat
#'
#' A flavor of \code{testex} that will inject \code{.Last.value} into the first
#' argument of each expression - suitable for using the \code{expect_*} family
#' of functions from \code{testthat}. Also handles temporarily attaching the
#' \code{testthat} package.
#'
#' @rdname testex_testthat
#' @export
testthat_expect <- function(..., val, envir = parent.frame()) {
  if (missing(val)) val <- quote(.Last.value)
  else val <- substitute(val)

  exprs <- substitute(...())
  exprs <- lapply(exprs, function(expr) {
    as.call(append(as.list(expr), list(quote(`.`)), after = 1L))
  })

  expr <- bquote({
    . <- .(val)
    invisible(.)
  })

  expr <- as.call(append(as.list(expr), exprs, after = 2L))
  expr <- bquote(local(testex::with_attached("testthat", .(expr))))
  eval(expr, envir = envir)
}



#' Helpers for using testex with testthat
#'
#' A flavor of \code{testex} that will inject \code{.Last.value} into the first
#' argument of each expression - suitable for using the \code{expect_*} family
#' of functions from \code{testthat}. Also handles temporarily attaching the
#' \code{testthat} package.
#'
#' @rdname testex_testthat
#' @export
testthat_block <- function(..., val, envir = parent.frame()) {
  if (missing(val)) val <- quote(.Last.value)
  else val <- substitute(val)

  exprs <- substitute(...())
  expr <- bquote({
    . <- .(val)
    invisible(.)
  })

  expr <- as.call(append(as.list(expr), exprs, after = 2L))
  expr <- bquote(local(testex::with_attached("testthat", .(expr))))
  eval(expr, envir = envir)
}



#' @export
expect_no_example_failure <- function(object, ...) {
  object <- substitute(object)
  act <- list(
    val = tryCatch(eval(object, envir = parent.frame()), error = identity),
    lab = deparse(object)
  )

  testthat::expect(
    !inherits(act$val, "error"),
    sprintf("Example %s threw an error during execution.", act$lab)
  )

  invisible(act$val)
}


#' @export
test_examples_as_testthat <- function(package, path = getwd(), ...,
  reporter = testthat::get_reporter(), envir = parent.frame()) {

  if (!missing(package)) {
    package_path <- find.package(package, quiet = TRUE)
    package_man <- file.path(package_path, "man")
    if (isTRUE(dir.exists(package_man))) rds <- tools::Rd_db(dir = package_path)
    else rds <- tools::Rd_db(package)
  } else {
    path <- find_package_root(path)
    package <- read.dcf(path, fields = "Package")[[1L]]
    rds <- tools::Rd_db(dir = path)
  }

  # create a temporary directory to store example tests
  testdir <- tempfile("testex")
  dir.create(testdir)

  wd <- getwd()
  setwd(testdir)
  on.exit(setwd(wd))

  rd_examples <- lapply(rds, function(rd) {
    rd_tags <- vapply(rd, attr, character(1L), "Rd_tag")
    rd_ex <- which(rd_tags == "\\examples")
    if (length(rd_ex) == 0L) return(NULL)
    rd[[rd_ex]]
  })

  rd_examples <- Filter(Negate(is.null), rd_examples)
  names(rd_examples) <- sprintf(
    "test-%s.Rd.R",
    tools::file_path_sans_ext(names(rd_examples))
  )

  for (i in seq_along(rd_examples)) {
    rd_filename <- names(rd_examples[i])
    rd_example <- rd_examples[[i]]
    example_code <- paste(unlist(rd_example), collapse = "")

    # inject manual .Last.value assignment to mimic example environment
    example_exprs <- parse(text = example_code)
    example_exprs <- lapply(example_exprs, function(expr) {
      bquote(.Last.value <<- testex::expect_no_example_failure(.(expr)))
    })

    # open up base so that we can assign to global .Last.value...
    # TODO: find another way to do this that isn't so heinous
    example_exprs <- append(
      example_exprs,
      list(
        bquote(library(.(package))),
        quote(unlockBinding(".Last.value", getNamespace("base")))
      ),
      after = 0L
    )

    example_code <- lapply(example_exprs, function(expr) {
      paste0(deparse(expr), collapse = "\n")
    })

    path <- file.path(testdir, rd_filename)
    writeLines(paste(example_exprs, collapse = "\n\n"), path)
    testthat:::test_one_file(basename(path))
  }
}
