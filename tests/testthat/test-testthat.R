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
    "<test>:1"  # error reported at "start" of srcref
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
    expect_no_error(stop(1)),
    class = "expectation"
  )

  expect_silent(expect_no_error(1 + 2))
})

test_that("testthat_block returns last value from previous expression", {
  expect_silent({
    ..Last.value <- 3
    out <- testthat_block(expect_equal(., 3), value = ..Last.value)
  })

  expect_equal(out, 3)
})

test_that("testthat_block skips if example throws error", {
  expect_silent({
    cond <- tryCatch({
        ..Last.value <- errorCondition("whoops!")
        testthat_block(expect_equal(., 3), value = ..Last.value)
      },
      condition = identity
    )
  })

  expect_s3_class(cond, "skip")
})

test_that("test_examples_as_testthat converts examples to tests and executes test suite", {
  dir.create(test_lib <- tempfile("testex_test_lib"))
  ex_pkg_path <- system.file(package = "testex", "pkg.example")
  install.packages(ex_pkg_path, lib = test_lib, repos = NULL, type = "source", quiet = TRUE)
  withr::defer(unlink(test_lib, recursive = TRUE))

  expect_silent({
    res <- callr::r(
      function() {
        library(testthat)
        library(testex)
        library(pkg.example)

        with_reporter(ListReporter$new(), {
          test_examples_as_testthat(path = find.package("pkg.example"))
        })
      },
      libpath = c(test_lib, .libPaths()),
      env = c(TESTTHAT = "true")
    )

    test_res <- res$results$as_list()
  })

  # resurface tests from example package as integration tests of testthat eval
  for (test in test_res) {
    for (expectation in test$results) {
      testthat::exp_signal(expectation)
    }
  }
})
