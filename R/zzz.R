.onLoad <- function(libname, pkgname) {
  s3_register("roxygen2::roclet_output", "roclet_rd")
  s3_register("roxygen2::roclet_process", "roclet_rd")
  s3_register("roxygen2::roxy_tag_parse", "roxy_tag_expect")
  s3_register("roxygen2::roxy_tag_parse", "roxy_tag_testthat")
}
