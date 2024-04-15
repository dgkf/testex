test_that("with_attached temporarily attaches a packages", {
  expect_true(suppressPackageStartupMessages({
    with_attached("roxygen2", any(grepl("roxygen2", search())))
  }))

  expect_true(!any(grepl("roxygen2", search())))
})

test_that("with_srcref binds srcref to testthat condition expectations", {
  as.srcref.character("<test>:1:2")

  expect_match(
    paste(collapse = "\n", capture.output(
      with_reporter(LocationReporter$new(), {
        test_that("example", with_srcref("<test>:1:2", expect_true(FALSE)))
      })
    )),
    "<test>:1" # error reported at "start" of srcref
  )
})

test_that("wrap_expect_no_error adds srcref, wraps code in expect_no_error expectation and assigns result to value", {
  expr <- quote(1 + 2)
  attr(expr, "srcref") <- srcref(srcfile("<text>"), 1:4)

  expect_silent(res_expr <- wrap_expect_no_error(expr, value = quote(..Last.value)))
  res_str <- paste0(deparse(res_expr), collapse = "\n")

  # unexpected whitespace may be introduced between langauge elements, due to
  # covr traces in quoted code
  expect_match(res_str, "testthat::test_that")
  expect_match(res_str, "testex::with_srcref(\"<text>:1:3\"", fixed = TRUE)
  expect_match(res_str, "..Last.value\\s+<<-\\s+")
  expect_match(res_str, "testthat::expect_no_error\\(\\s*1\\s+\\+\\s+2")
})

test_that("expect_no_error reports when testthat errors occurs while evaluating an expression", {
  expect_condition(
    fallback_expect_no_error(stop(1)),
    class = "expectation"
  )

  expect_silent(fallback_expect_no_error(1 + 2))
})

test_that("testthat_block returns last value from previous expression", {
  expect_silent(withr::with_dir(pkg_example_dir, as_not_r_cmd_check({
    ..Last.value <- 3
    out <- testex(style = "testthat", expect_equal(., 3), value = ..Last.value)
  })))

  expect_equal(out, 3)
})

test_that("testthat_block skips if example throws error", {
  expect_silent(withr::with_dir(pkg_example_dir, as_not_r_cmd_check({
    cond <- tryCatch(
      {
        ..Last.value <- errorCondition("whoops!")
        testex(style = "testthat", expect_equal(., 3), value = ..Last.value)
      },
      condition = identity
    )
  })))

  expect_s3_class(cond, "skip")
})
