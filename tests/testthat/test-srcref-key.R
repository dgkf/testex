# first expression, used for testing
{
  1 + 2
}

test_that("srcrefs can round-trip through a string srcref key", {
  this_file <- file.path(testthat::test_path(), "test-srcref-key.R")
  exprs <- parse(this_file, n = 1, keep.source = TRUE)
  expr <- exprs[[1]]
  attr(expr, "srcref") <- getSrcref(exprs)[[1]]

  # ensure key includes filename and start-end lines
  expect_equal(srcref_key(expr), "test-srcref-key.R:2:4")

  # srcref keys can be parsed back into srcrefs
  expect_equal(
    as.srcref(srcref_key(expr)),
    srcref(
      srcfilealias("test-srcref-key.R", srcfile("test-srcref-key.R")),
      c(2, 1, 4, 1)
    )
  )
})

test_that("srcrefs for package files find actual file full path", {
  src_file <- file.path(find.package("testex"), "tests", "testthat", "test-srcref-key.R")
  skip_if_not(file.exists(src_file), "tests not installed")

  exprs <- parse(src_file, n = 1, keep.source = TRUE)
  expr <- exprs[[1]]
  attr(expr, "srcref") <- getSrcref(exprs)[[1]]

  # ensure key includes filename and start-end lines
  expect_match(srcref_key(expr), "test-srcref-key.R:\\d+:\\d+")

  # srcref key file paths can be absolute when a package root is found
  expect_true(!is.null(find_package_root(quiet = FALSE)))
  expect_silent(src_key_file <- getSrcFilename(as.srcref(srcref_key(expr, path = "full")), full.names = TRUE))
  expect_equal(src_key_file, tools::file_path_as_absolute(src_key_file))
})

test_that("srcref keys can be customized to include more detailed locations", {
  src_file <- file.path(find.package("testex"), "fakedir", "file.R")
  expr <- expression(1)
  attr(expr, "srcref") <- srcref(srcfile(src_file), 1:8)

  # ensure key includes filename and start-end lines
  expect_match(srcref_key(expr, nloc = 4), ".*(:\\d+){4}")
  expect_equal(as.numeric(as.srcref(srcref_key(expr, nloc = 4)))[1:4], 1:4)
  expect_match(srcref_key(expr, nloc = 6), ".*(:\\d+){6}")
  expect_equal(as.numeric(as.srcref(srcref_key(expr, nloc = 6)))[1:6], 1:6)
  expect_match(srcref_key(expr, nloc = 8), ".*(:\\d+){8}")
  expect_equal(as.numeric(as.srcref(srcref_key(expr, nloc = 8)))[1:8], 1:8)
})


test_that("srcrefs using root package path produce full paths", {
  src_file <- file.path(find.package("testex"), "fakedir", "file.R")
  expr <- expression(1)
  attr(expr, "srcref") <- srcref(srcfile(src_file), 1:8)

  expect_silent(src_key <- srcref_key(expr, path = "base"))
  expect_equal(gsub(":.*", "", src_key), "file.R")
  expect_silent(src_key_file <- getSrcFilename(as.srcref(src_key), full.names = TRUE))
  expect_equal(src_key_file, "file.R")

  expect_silent(src_key <- srcref_key(expr, path = "root"))
  expect_equal(gsub(":.*", "", src_key), file.path("fakedir", "file.R"))
  expect_silent(src_key_file <- getSrcFilename(as.srcref(src_key), full.names = TRUE))
  expect_equal(src_key_file, file.path("fakedir", "file.R"))

  expect_silent(src_key <- srcref_key(expr, path = "full"))
  expect_match(src_key, src_file, fixed = TRUE)
  expect_silent(src_key_file <- getSrcFilename(as.srcref(src_key), full.names = TRUE))
  expect_equal(src_key_file, src_file)
})
