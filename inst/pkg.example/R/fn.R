#' Test Function
#'
#' This example showcases how you might write "raw" tests within your examples.
#' You could use `\testonly` directly, or use `testex::testex()` to use
#' `.`-syntax.
#'
#' @param x A thing
#'
#' @return The pasted thing
#'
#' @examples
#' fn("testing")
#'
#' \testonly{testex::testex(
#'   . == "testing 1 2 3",
#'   startsWith(., "testing")
#' )}
#'
#' \testonly{testex::testex(style = "testthat",
#'   testthat::test_that("fn gives expected results", {
#'     testthat::expect_equal(., "testing 1 2 3")
#'   })
#' )}
#'
#' @export
fn <- function(x) {
  paste(x, "1 2 3")
}



#' Test Function
#'
#' This example introduces the `@test` tag, either a value or an expression
#' using the `.`-syntax to test the last example result.
#'
#' @param x A thing
#'
#' @return The pasted thing
#'
#' @examples
#' \dontshow{
#'   value <- "testing"
#' }
#'
#' fn_roxygen(value)
#' @test "testing 1 2 3"
#'
#' \dontrun{
#'   stop("this won't work")
#' }
#'
#' fn_roxygen("testing")
#' @test grepl("\\d", .)
#' @test startsWith(., "testing")
#'
#' fn_roxygen("testing")
#' @test {
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
#' This example introduces `testthat`-style tests using in-line `@testthat`
#' `roxygen2` tags.
#'
#' @param x A thing
#'
#' @return The pasted thing
#'
#' @examples
#' fn_roxygen_testthat("testing")
#' @testthat expect_equal("testing 1 2 3")
#' @testthat expect_match("^testing")
#'
#' fn_roxygen_testthat("testing")
#' @testthat expect_equal("testing 1 2 3")
#' @testthat expect_match("^testing")
#'
#' @export
fn_roxygen_testthat <- function(x) {
  paste(x, "1 2 3")
}




#' Test Topic Covering Multiple Functions
#'
#' This example composes an examples section from multiple blocks.
#'
#' @param x A thing
#' @return The pasted thing
#'
#' @name fn_roxygen_multiple
NULL

#' @describeIn fn_roxygen_multiple
#' Ensure multiple objects' examples are combined into a single topic
#'
#' @examples
#' fn_roxygen_multiple1("testing")
#' @test grepl("\\d", .)
#' @test startsWith(., "testing 1 2 3")
#'
#' @export
fn_roxygen_multiple1 <- function(x) {
  paste(x, "1 2 3")
}

#' @describeIn fn_roxygen_multiple
#' Ensure multiple objects' examples are combined into a single topic
#'
#' @examples
#' fn_roxygen_multiple2("testing")
#' @test grepl("\\d", .)
#' @test startsWith(., "testing")
#'
#' @export
fn_roxygen_multiple2 <- function(x) {
  paste(x, "1 2 3")
}
