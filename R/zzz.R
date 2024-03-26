.onLoad <- function(libname, pkgname) {
  s3_register("roxygen2::roxy_tag_parse", "roxy_tag_test")
  s3_register("roxygen2::roxy_tag_parse", "roxy_tag_testthat")
}
