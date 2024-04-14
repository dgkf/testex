test_that("testex code blocks evaluate expectations against target symbol", {
  # style = I("standalone") used to avoid converting test style to accommodate
  # running testthat suite.

  expect_silent({
    ..Last.value <- 3
    testex(style = I("standalone"), identical(., 3), value = quote(..Last.value))
  })

  expect_error({
    ..Last.value <- 3
    testex(style = I("standalone"), identical(., 4), value = quote(..Last.value))
  })
})
