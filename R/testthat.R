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
test_examples <- function(package,
  path = if (missing(package)) getwd() else find.package(package, quiet = TRUE),
  ...) {

  path <- find_package_root(path)
  rds <- tools::Rd_db(dir = path)

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

  for (i in seq_along(rd_examples)) {
    rd_filename <- names(rd_examples[i])
    rd_example <- rd_examples[[i]]
    path <- file.path(testdir, rd_filename)
    writeLines(paste(unlist(rd_example), collapse = ""), path)
  }

  testthat:::test_files(
    test_dir = testdir,
    test_paths = names(rd_examples),
    test_package = packageName(),
    ...,
    load_package = "installed"
  )
}
