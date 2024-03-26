test_that("@test tags produce \\testonly blocks", {
  roxy_text <- "
    #' Title
    #'
    #' Description.
    #'
    #' @param x,y parameters
    #'
    #' @examples
    #' 1 + 2
    #' @test 3
    #'
    #' @export
    f <- function(x, y) x + y
  "

  block <- roxygen2::parse_text(roxy_text)[[1]]
  expect_tag <- block$tags[[5]]

  expect_equal(expect_tag$tag, "test")
  expect_s3_class(expect_tag, "roxy_tag_examples")

  expect_true(any(grepl("\\\\testonly\\{", expect_tag$val)))
  expect_true(any(grepl("testex::testex\\(", expect_tag$val)))
  expect_true(any(grepl("identical\\(\\., 3\\)", expect_tag$val)))
})
