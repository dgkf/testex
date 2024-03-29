% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roxygen2.R
\name{testex-roxygen-tags}
\alias{testex-roxygen-tags}
\title{\code{\link{testex}} \code{roxygen2} tags}
\description{
\code{\link{testex}} provides two new \code{roxygen2} tags, \verb{@test} and \verb{@testthat}.
}
\section{tags}{

\link{testex} tags are all sub-tags meant to be used within an
\verb{@examples} block. They should be considered as tags \emph{within} the
\verb{@examples} block and used to construct blocks of testing code within
example code.

\describe{
\item{\verb{@test}: }{
In-line expectations to test the output of the previous command within an
example. If \code{.} is used within the test expression, it will be used to
refer to the output of the previous example command. Otherwise, the
result of the expression is expected to be identical to the previous
output.

\if{html}{\out{<div class="sourceCode">}}\preformatted{#' @examples
#' 1 + 2
#' @test 3
#' @test . == 3
#'
#' @examples
#' 3 + 4
#' @test identical(., 7)
}\if{html}{\out{</div>}}

}
}

\describe{
\item{\verb{@testthat}: }{
Similar to \verb{@test}, \verb{@testthat} can be used to make in-line
assertions using \code{testthat} expectations. \code{testthat} expectations
follow a convention where the first argument is an object to compare
against an expected value or characteristic. Since the value will always
be the result of the previous example, this part of the code is
implicitly constructed for you.

If you want to use the example result elsewhere in your expectation, you
can refer to it with a \code{.}. When used in this way, \link{testex} will
not do any further implicit modification of your expectation.

\if{html}{\out{<div class="sourceCode">}}\preformatted{#' @examples
#' 1 + 2
#' @testthat expect_equal(3)
#' @testthat expect_gt(0)
#'
#' @examples
#' 3 + 4
#' @testthat expect_equal(., 7)
}\if{html}{\out{</div>}}

}
}
}

