test_that("update_testex_desc reads and caches options set in Config/testex/options", {
  desc <- "
Package: example
Config/testex/options: list(a = 1)
  "

  dir.create(test_dir <- tempfile("testex"))
  desc_path <- file.path(test_dir, "DESCRIPTION")
  withr::defer(unlink(test_dir, recursive = TRUE))

  writeLines(trimws(desc), desc_path)
  expect_silent(testex_options(desc_path))
  expect_silent(orig <- testex_options(""))
  expect_length(orig, 1)
  expect_identical(orig$a, 1)
  orig_mtime <- .testex_options$.fingerprint$mtime

  # during R CMD check, process ID is used for fingerprint instead of mtime
  skip_if(is_r_cmd_check(), "on R CMD check")

  # without updating file, cache fingerprint unchanged
  expect_silent(testex_options(desc_path))
  expect_true(.testex_options$.fingerprint$mtime == orig_mtime)

  desc <- "
Package: example
Config/testex/options: list(a = 1, b = 2)
  "

  # expect invalidation of cached value and new values stored
  writeLines(trimws(desc), desc_path)
  expect_silent(testex_options(desc_path))
  expect_silent(updated <- testex_options(""))
  expect_true(.testex_options$.fingerprint$mtime != orig_mtime)
  expect_length(updated, 2)
  expect_identical(updated$a, 1)
  expect_identical(updated$b, 2)
})
