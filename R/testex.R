#' A syntactic helper for writing quick and easy example tests
#'
#' A wrapper around `stopifnot` that allows you to use `.` to refer to
#' `.Last.value` and preserve the last non-test output from an example.
#'
#' @section Documenting with `testex`:
#'
#' `testex` is a simple wrapper around execution that propegates the
#' `.Last.value` returned before running, allowing you to chain expectations
#' more easily.
#'
#' ## Use in `Rd` files:
#'
#' \preformatted{
#' \examples{
#'   f <- function(a, b) a + b
#'   f(3, 4)
#'   \testonly{
#'     testex::testex(
#'       is.numeric(.),
#'       identical(., 7)
#'     )
#'   }
#' }
#' }
#'
#' But `Rd` files are generally regarded as being a bit cumbersome to author
#' directly. Instead, `testex` provide helpers that generate this style of
#' documentation, which use this function internally.
#'
#' ## Use with `roxygen2`
#'
#' Within a `roxygen2` `@examples` block you can instead use the `@expect` tag
#' which will generate Rd code as shown above.
#'
#' \preformatted{
#' #' @examples
#' #' f <- function(a, b) a + b
#' #' f(3, 4)
#' #' @expect is.numeric(.)
#' #' @expect identical(., 7)
#' }
#'
#' @param ... Expressions to evaluated. \code{.} will be replaced with the
#'   expression passed to \code{val}, and may be used as a shorthand for the
#'   last example result.
#' @param value A value to test against. By default, this will use the example's
#'   \code{.Last.value}.
#' @param obj An optional object name used to construct a more helpful error
#'   message testthat failure message.
#' @param example An option `srcref_key` string used to indicate where the
#'   relevant example code originated from.
#' @param tests An option `srcref_key` string used to indicate where the
#'   relevant test code originated from.
#' @param envir An environment in which tests should be evaluated. By default
#'   the parent environment where tests are evaluated.
#'
#' @return Invisibly returns the `.Last.value` as it existed prior to evaluating
#'   the test.
#'
#' @export
testex <- function(..., value = get_example_value(), obj = NULL,
  example = NULL, tests = NULL, envir = parent.frame()) {

  if (is_r_cmd_check() && isFALSE(testex_options()$check)) {
    return(invisible(.Last.value))
  }

  if (!missing(value)) value <- substitute(value)

  exprs <- substitute(...())
  exprs <- lapply(exprs, function(expr) {
    eval(bquote(substitute(.(expr), list(`.` = .(value)))))
  })

  expr <- as.call(append(list(as.name("stopifnot")), exprs))
  expr <- bquote({
    .(expr)
    invisible(.Last.value)
  })

  eval(expr, envir = envir)
}
