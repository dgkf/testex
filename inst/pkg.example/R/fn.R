#' Test Function
#'
#' @param x A thing
#'
#' @examples
#' fn("testing")
#'
#' \testonly{testex::testex(
#'   . == "testing 1 2 3",
#'   startsWith(., "testing")
#' )}
#'
#' \testonly{testex::testthat_expect(
#'   expect_equal("testing 1 2 3")
#' )}
#'
#' \testonly{testex::testthat_block(
#'   test_that("fn gives expected results", {
#'     expect_equal(., "testing 1 2 3")
#'   })
#' )}
#'
#' @export
fn <- function(x) {
  paste(x, "1 2 3")
}



#' Test Function
#'
#' @param x A thing
#'
#' @examples
#' fn_roxygen("testing")
#' @expect "testing 1 2 3"
#'
#' fn_roxygen("testing")
#' @expect grepl("\\d", .)
#' @expect startsWith(., "testing")
#'
#' fn_roxygen("testing")
#' @expect {
#'   "testing 1 2 3"
#' }
#'
#' fn_roxygen("testing")
#' # untested trailing example
#'
#' @export
fn_roxygen <- function(x) {
  paste(x, "1 2 3")
}



#' Test Function
#'
#' @param x A thing
#'
#' @examples
#' fn_roxygen("testing")
#' @testthat expect_equal("testing 1 2 3")
#' @testthat expect_match("^tasting")
#'
#' fn_roxygen(stop(3))
#' @testthat expect_equal("testing 1 2 3")
#' @testthat expect_match("^testing")
#'
#' @export
fn_roxygen_testthat <- function(x) {
  paste(x, "1 2 3")
}
