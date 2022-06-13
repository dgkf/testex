#' Helpers for using testex with testthat
#'
#' A flavor of \code{testex} that will inject \code{.Last.value} into the first
#' argument of each expression - suitable for using the \code{expect_*} family
#' of functions from \code{testthat}. Also handles temporarily attaching the
#' \code{testthat} package.
#'
#' @param ... Expectations to evaluate with \pkg{testthat}
#' @inheritParams testex
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



#' testthat expectation asserting that code executes without error
#'
#' @param object An expression to evaluate
#' @param ... Additional arguments unused
#'
#' @export
expect_no_example_error <- function(object, ...) {
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



#' Execute examples from Rd files as testthat tests
#'
#' Reads examples from Rd files and constructs \pkg{testthat}-style tests. Each
#' expression is expected to run without error and any in-line expectations
#' naturally result in individual expectations.
#'
#' @param package A package name whose examples should be tested
#' @param path Optionally, a path to a source code directory to use. Will only
#'   have an effect if parameter \code{package} is missing.
#' @param ... Additional argument unused
#' @param reporter A \pkg{testthat} reporter to use. Defaults to the active
#'   reporter in the \pkg{testthat} environment or default reporter.
#'
#' @export
test_examples_as_testthat <- function(package, path = getwd(), ...,
  reporter = testthat::get_reporter()) {

  requireNamespace("testthat")

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

  rd_examples_testfiles <- lapply(seq_along(rd_examples), function(i) {
    rd_filename <- names(rd_examples[i])
    rd_example <- rd_examples[[i]]
    example_code <- paste(unlist(rd_example), collapse = "")

    # inject manual .Last.value assignment to mimic example environment
    example_exprs <- parse(text = example_code)
    example_exprs <- lapply(example_exprs, function(expr) {
      bquote(.Last.value <<- testex::expect_no_example_error(.(expr)))
    })

    # open up base so that we can assign to global .Last.value...
    # TODO: find another way to do this that isn't so heinous
    example_exprs <- append(
      example_exprs,
      list(
        call("library", package),
        call("unlockBinding", ".Last.value", quote(getNamespace("base")))
      ),
      after = 0L
    )

    example_code <- lapply(example_exprs, function(expr) {
      paste0(deparse(expr), collapse = "\n")
    })

    path <- file.path(testdir, rd_filename)
    writeLines(paste(example_exprs, collapse = "\n\n"), path)
    path
  })

  if (bindingIsLocked(".Last.value", getNamespace("base"))) {
    on.exit(lockBinding(".Last.value", getNamespace("base")), add = TRUE)
  }

  for (file in rd_examples_testfiles) {
    testthat::context_start_file(basename(file))
    testthat::source_file(file)
  }
}
