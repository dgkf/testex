as_r_cmd_check <- function(expr, pkg = "pkg") {
  withr::with_envvar(
    list("_R_CHECK_PACKAGE_NAME_" = pkg),
    expr
  )
}

as_not_r_cmd_check <- function(expr) {
  withr::with_envvar(
    list("_R_CHECK_PACKAGE_NAME_" = NA),
    expr
  )
}
