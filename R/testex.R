#' A syntactic helper for writing quick and easy example tests
#'
#' A wrapper around \code{stopifnot} that allows you to use \code{.} to refer to
#' \code{.Last.value} and preserve the last non-test output from an example.
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
#' @return invisibly returns the `.Last.value` as it existed prior to evaluating
#'   the test
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
