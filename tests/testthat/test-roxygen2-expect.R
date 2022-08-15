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
    #' @expect 3
    #'
    #' @export
    f <- function(x, y) x + y
  "

  block <- roxygen2::parse_text(roxy_text)[[1]]

  expect_silent({
    block <- roclet_process_testex(block)
    ex_idx <- which(vcapply(block$tags, `[[`, "tag") == "examples")
    ex_tag <- block$tags[[ex_idx]]
    ex_val <- ex_tag$val
  })

  expect_true(any(grepl("\\\\testonly\\{", ex_tag$val)))
  expect_true(any(grepl("testex::testex\\(", ex_tag$val)))
  expect_true(any(grepl("identical\\(\\., 3\\)", ex_tag$val)))
  expect_true(any(grepl("example = \".*:.:.\"", ex_tag$val)))
})
