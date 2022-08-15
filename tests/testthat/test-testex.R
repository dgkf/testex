test_that("testex code blocks evaluate expectations against target symbol", {
  expect_silent({
    ..Last.value <- 3
    testex(identical(., 3), value = quote(..Last.value))
  })

  expect_error({
    ..Last.value <- 3
    testex(identical(., 4), value = quote(..Last.value))
  })
})
