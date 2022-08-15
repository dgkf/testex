test_that("roxygen2 can parse testex tags without raising conditions", {
  roxy_text <- "
    #' Title
    #'
    #' Description.
    #'
    #' @param x,y parameters
    #'
    #' @examples
    #' 1 + 2
    #' @expect 3
    #'
    #' @export
    f <- function(x, y) x + y
  "

  expect_silent(block <- roxygen2::parse_text(roxy_text)[[1]])
})
