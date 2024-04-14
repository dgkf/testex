test_that("@expect tags produce \\testonly blocks", {
  roxy_text <- "
    #' Title
    #'
    #' Description.
    #'
    #' @param x,y parameters
    #'
    #' @examples
    #' 1 + 2
    #' @testthat expect_equals(3)
    #'
    #' @export
    f <- function(x, y) x + y
  "

  block <- roxygen2::parse_text(roxy_text)[[1]]
  testthat_tag <- block$tags[[5]]

  expect_equal(testthat_tag$tag, "testthat")
  expect_s3_class(testthat_tag, "roxy_tag_examples")

  expect_true(any(grepl("\\\\testonly\\{", testthat_tag$val)))
  expect_true(any(grepl("testex::testex\\(", testthat_tag$val)))
  expect_true(any(grepl("expect_equals\\(\\., 3\\)", testthat_tag$val)))
})
