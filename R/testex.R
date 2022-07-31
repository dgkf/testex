#' A syntactic helper for writing quick and easy example tests
#'
#' A wrapper around \code{stopifnot} that allows you to use \code{.} to refer to
#' \code{.Last.value} and preserve the last non-test output from an example.
#'
#' @param ... Expressions to evaluated. \code{.} will be replaced with the
#'   expression passed to \code{val}, and may be used as a shorthand for the
#'   last example result.
#' @param val A value to test against. By default, this will use the example's
#'   \code{.Last.value}.
#' @param envir An environment in which tests should be evaluated. By default
#'   the parent environment where tests are evaluated.
#'
#' @rdname testex
#' @export
testex <- function(..., val, source = NULL, envir = parent.frame()) {
  if (is_r_cmd_check() && isFALSE(testex_options()$check)) {
    return(invisible(.Last.value))
  }

  if (missing(val)) val <- quote(quote(.Last.value))
  else val <- substitute(val)

  exprs <- substitute(...())
  exprs <- lapply(exprs, function(expr) {
    eval(bquote(substitute(.(expr), list(`.` = .(val)))))
  })

  expr <- as.call(append(list(as.name("stopifnot")), exprs))
  expr <- bquote({
    .(expr)
    invisible(.Last.value)
  })

  eval(expr, envir = envir)
}
