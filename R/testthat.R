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
testthat_expect <- function(..., value = get_example_value(),
  envir = parent.frame()) {

  if (!missing(value)) value <- substitute(value)

  exprs <- substitute(...())
  exprs <- lapply(exprs, function(expr) {
    as.call(append(as.list(expr), list(quote(`.`)), after = 1L))
  })

  expr <- bquote({
    . <- .(value)
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
testthat_block <- function(..., value = get_example_value(), obj = NULL,
  example = NULL, tests = NULL, envir = parent.frame()) {

  if (!missing(value)) value <- substitute(value)

  exprs <- substitute(...())
  expr <- bquote({
    . <- .(value)
    skip_if(inherits(., "error"), "previous example produced an error")
    invisible(.)
  })

  expr <- as.call(append(as.list(expr), exprs, after = 3L))
  expr <- bquote(local(testex::with_attached("testthat", .(expr))))
  eval(expr, envir = envir)
}



#' @export
with_srcref <- function(src, expr, envir = parent.frame()) {
  expr <- substitute(expr)
  withCallingHandlers(
    eval(expr, envir = envir),
    expectation = function(e) {
      e[["srcref"]] <-as.srcref(src)
      testthat::exp_signal(e)
      invokeRestart(computeRestarts()[[1L]])
    }
  )
}



#' testthat expectation asserting that code executes without error
#'
#' @param object An expression to evaluate
#' @param ... Additional arguments unused
#'
#' @export
expect_no_error <- function(object, ...) {
  object <- substitute(object)
  act <- list(
    val = tryCatch(eval(object, envir = parent.frame()), error = identity),
    lab = deparse(object)
  )

  testthat::expect(
    !inherits(act$val, "error"),
    failure_message = sprintf("Example %s threw an error during execution.", act$lab),
    ...
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
test_examples_as_testthat <- function(package, path, ...,
  test_dir = tempfile("testex"), quiet = TRUE, clean = TRUE, overwrite = TRUE,
  reporter = testthat::get_reporter()) {

  requireNamespace("testthat")

  if (missing(path))
    path <- find_package_root(testthat::test_path())

  rds <- find_package_rds(package, path)
  test_dir_exists <- dir.exists(test_dir)

  if (!test_dir_exists) {
    dir.create(test_dir)
    if (clean) on.exit(unlink(test_dir))
  }

  if (test_dir_exists && !overwrite)  {
    test_files <- list.files(test_dir, full.names = TRUE)
    test_files(test_files, chdir = FALSE, "examples [run from testex]")
    return()
  }

  # find example sections and conver them to tests
  rd_examples <- Filter(Negate(is.null), lapply(rds, rd_extract_examples))
  test_files <- lapply(seq_along(rd_examples), function(i) {
    rd_filename <- names(rd_examples[i])
    rd_example <- rd_examples[[i]]

    # break up examples into examples and test, wrap examples in expectations
    exprs <- split_testonly_as_expr(rd_example)
    is_ex <- names(exprs) != "\\testonly"
    exprs[is_ex] <- lapply(
      exprs[is_ex],
      wrap_expect_no_error,
      value = quote(..Last.value)  # can't use base::.Last.value in testthat env
    )

    # write out test code to file in test dir
    path <- file.path(test_dir, paste0(tools::file_path_sans_ext(rd_filename), ".R"))
    example_code <- vcapply(exprs, deparse_pretty)

    writeLines(paste(example_code, collapse = "\n\n"), path)
    path
  })

  test_files(test_files, chdir = FALSE, "examples [run from testex]")
}



deparse_pretty <- function(expr) {
  lines <- deparse(expr, width.cutoff = 120L)
  paste0(gsub("^(  +)\\1", "\\1", lines), collapse = "\n")
}



test_files <- function(files, context, ...) {
  testthat::context_start_file(context)
  for (file in files) testthat::source_file(file, ...)
}



#' Wraps an example expression in a testthat expectation to not error
#'
wrap_expect_no_error <- function(expr, value) {
  srckey <- srcref_key(expr, path = "root")
  bquote(testthat::test_that("example executes without error", {
    testex::with_srcref(.(srckey), {
      .(value) <<- testex::expect_no_error(.(expr))
    })
  }))
}



#' Determine which symbol to use by default when testing examples
#'
get_example_value <- function() {
  if (testthat::is_testing()) quote(..Last.value)
  else quote(.Last.value)
}
