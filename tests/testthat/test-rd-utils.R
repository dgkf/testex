test_that("rd_code_as_string collapses code into string", {
  rd_raw <- "
  \\examples{
  1 + 2

  4
  }
  "

  dir.create(test_dir <- tempfile("testex"))
  test_path <- file.path(test_dir, "rd")
  withr::defer(unlink(test_dir, recursive = TRUE))

  writeLines(rd_raw, test_path)
  rd <- tools::parse_Rd(test_path)

  expect_silent(rd_string <- rd_code_as_string(rd))
  expect_match(rd_string, "1 \\+ 2\n\n[ ]*4")
})

test_that("rd_extract_examples pulls out \\examples Rd tags", {
  rd_raw <- "
  \\title{test}
  \\description{testing!}
  \\examples{
  1 + 2
  \\testonly{
    identical(.Last.value, 3)
  }

  4
  }
  "

  dir.create(test_dir <- tempfile("testex"))
  test_path <- file.path(test_dir, "rd")
  withr::defer(unlink(test_dir, recursive = TRUE))

  writeLines(rd_raw, test_path)
  rd <- tools::parse_Rd(test_path)

  expect_silent(examples_rd <- rd_extract_examples(rd))
  expect_equal(attr(examples_rd, "Rd_tag"), "\\examples")
})

test_that("split_testonly splits examples block into components", {
  rd_raw <- "\\examples{
  1 + 2
  \\testonly{
    identical(.Last.value, 3)
  }

  3 + 4
  \\testonly{
    identical(.Last.value, 7)
  }

  5
  }"

  dir.create(test_dir <- tempfile("testex"))
  test_path <- file.path(test_dir, "rd")
  withr::defer(unlink(test_dir, recursive = TRUE))

  writeLines(rd_raw, test_path)
  rd <- tools::parse_Rd(test_path)[[1]]

  expect_silent(rd_example_sections <- split_testonly(rd))
  expect_length(rd_example_sections, 5)
  expect_named(rd_example_sections)
  expect_equal(names(rd_example_sections), c("RCODE", "\\testonly", "RCODE", "\\testonly", "RCODE"))
})

test_that("split_testonly_as_expr parses example components and preserves srcrefs", {
  rd_raw <- "\\examples{
  1 + 2
  \\testonly{
    identical(.Last.value, 3)
  }

  3 + 4
  \\testonly{
    identical(.Last.value, 7)
  }

  5
  }"

  dir.create(test_dir <- tempfile("testex"))
  test_path <- file.path(test_dir, "rd")
  withr::defer(unlink(test_dir, recursive = TRUE))

  writeLines(rd_raw, test_path)
  rd <- tools::parse_Rd(test_path)[[1]]

  expect_silent(rd_example_sections <- split_testonly_as_expr(rd))
  expect_length(rd_example_sections, 5)
  expect_true(all(vlapply(rd_example_sections, is.language) | vlapply(rd_example_sections, is.atomic)))
  expect_true(!any(vlapply(lapply(rd_example_sections, getSrcref), is.null)))
})

test_that("split_testonly_as_expr handles \\dontrun, \\dontshow Rd tags", {
  rd_raw <- "\\examples{
  \\dontshow{
    print('hi mom!')
  }

  1 + 2
  \\testonly{
    identical(.Last.value, 3)
  }

  \\dontrun{
    stop('whoops!')
  }

  3 + 4
  \\testonly{
    identical(.Last.value, 7)
  }

  5
  }"

  dir.create(test_dir <- tempfile("testex"))
  test_path <- file.path(test_dir, "rd")
  withr::defer(unlink(test_dir, recursive = TRUE))

  writeLines(rd_raw, test_path)
  rd <- tools::parse_Rd(test_path)[[1]]

  expect_silent(rd_example_sections <- split_testonly_as_expr(rd))
  expect_length(rd_example_sections, 5)
  expect_named(rd_example_sections)
  expect_equal(names(rd_example_sections), c("RCODE", "\\testonly", "RCODE", "\\testonly", "RCODE"))
})
