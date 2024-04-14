test_that("test_examples_as_testthat converts examples to tests and executes test suite", {
  dir.create(test_lib <- tempfile("testex_test_lib"))
  withr::defer(unlink(test_lib, recursive = TRUE))

  install.packages(
    system.file(package = "testex", "pkg.example"),
    lib = test_lib,
    repos = NULL,
    type = "source",
    INSTALL_opts = "--install-tests",
    quiet = testthat::is_testing()
  )

  expect_silent({
    res <- callr::r(
      function() {
        library(testthat)
        library(testex)
        library(pkg.example)

        with_reporter(ListReporter$new(), {
          test_examples_as_testthat(
            path = find.package("pkg.example"),
            roxygenize = FALSE # pkgload works weirdly here...
          )
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
