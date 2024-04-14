test_that("testex code blocks evaluate expectations against target symbol", {
  # tag = "" used to avoid running @test as @testthat while testthat is running

  expect_silent({
    ..Last.value <- 3
    testex(tag = "", identical(., 3), value = quote(..Last.value))
  })

  expect_error({
    ..Last.value <- 3
    testex(tag = "", identical(., 4), value = quote(..Last.value))
  })
})
