test_that("escape_infotex doubles infotex backslashes", {
  expect_equal(escape_infotex("\\testonly"), "\\\\testonly")
})
