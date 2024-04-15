test_that("testex code blocks evaluate expectations against target symbol", {
  # style = I("standalone") used to avoid converting test style to accommodate
  # running testthat suite.

  expect_silent(withr::with_dir(pkg_example_dir, as_not_r_cmd_check({
    ..Last.value <- 3
    testex(style = I("standalone"), identical(., 3), value = ..Last.value)
  })))

  expect_error(withr::with_dir(pkg_example_dir, as_not_r_cmd_check({
    ..Last.value <- 3
    testex(style = I("standalone"), identical(., 4), value = ..Last.value)
  })))
})
