test_that(paste0(
  "use_testex adds testex to Suggests and Roxygen roclets specification ",
  "if it does not yet exist"
), {
  ex_pkg_inst <- system.file(package = "testex", "pkg.example")

  dir.create(test_dir <- tempfile("testex"))
  ex_pkg_path <- file.path(test_dir, basename(ex_pkg_inst))
  file.copy(ex_pkg_inst, test_dir, recursive = TRUE)

  desc <- "
Title: pkg.example
Version: 1.2.3
  "

  desc_path <- file.path(ex_pkg_path, "DESCRIPTION")
  writeLines(desc, desc_path)
  withr::defer(unlink(test_dir, recursive = TRUE))

  expect_equal(read.dcf(desc_path, fields = "Roxygen")[1,][[1]], NA_character_)
  expect_silent(use_testex(ex_pkg_path, quiet = TRUE))
  expect_match(read.dcf(desc_path, fields = "Roxygen")[1,][[1]], "^list\\(")
  expect_match(read.dcf(desc_path, fields = "Roxygen")[1,][[1]], "packages = \"testex\"")
  expect_match(read.dcf(desc_path, fields = "Suggests")[1,][[1]], "\\btestex\\b")
})

test_that("use_testex_as_testthat adds test-testex.R when testthat already in use", {
  ex_pkg_inst <- system.file(package = "testex", "pkg.example")

  dir.create(test_dir <- tempfile("testex"))
  ex_pkg_path <- file.path(test_dir, basename(ex_pkg_inst))
  file.copy(ex_pkg_inst, test_dir, recursive = TRUE)
  testthat_testex_file <- file.path(ex_pkg_path, "tests", "testthat", "test-testex.R")
  file.remove(testthat_testex_file)
  withr::defer(unlink(test_dir, recursive = TRUE))

  expect_silent(use_testex_as_testthat(ex_pkg_path))
  expect_true(file.exists(testthat_testex_file))
  expect_true(any(grepl("testex::test_examples", readLines(testthat_testex_file))))
})

test_that("use_testex_as_testthat aborts when testthat not in use", {
  ex_pkg_inst <- system.file(package = "testex", "pkg.example")

  dir.create(test_dir <- tempfile("testex"))
  ex_pkg_path <- file.path(test_dir, basename(ex_pkg_inst))
  file.copy(ex_pkg_inst, test_dir, recursive = TRUE)
  testthat_dir <- file.path(ex_pkg_path, "tests", "testthat")
  unlink(testthat_dir, recursive = TRUE)
  withr::defer(unlink(test_dir, recursive = TRUE))

  expect_error(use_testex_as_testthat(ex_pkg_path), "use testthat")
})
