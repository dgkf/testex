`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}



`%|NA|%` <- function(lhs, rhs) {
  if (is.na(lhs)) rhs else lhs
}



#' Temporarily attach a namespace
#'
#' @param ns A namespace or namespace name to attach
#' @param expr An expression to evaluate while the namespace is attached
#'
#' @export
with_attached <- function(ns, expr) {
  if (is.character(ns)) {
    requireNamespace(ns)
  }

  try({
    attached <- attachNamespace(ns)
    on.exit(detach(attr(attached, "name"), character.only = TRUE))
  }, silent = TRUE)

  expr <- substitute(expr)
  eval(expr)
}



is_r_cmd_check <- function() {
  !is.na(Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA_character_))
}



find_package_root <- function(path, quiet = FALSE) {
  while (dirname(path) != path) {
    if (file.exists(file.path(path, "DESCRIPTION")))
      return(path)

    if (path == ".") path <- getwd()
    path <- dirname(path)
  }

  if (!quiet) stop("Unable to find package root")
  invisible(NULL)
}



package_name <- function() {
  x <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA_character_)
  if (!is.na(x)) return(x)

  x <- find_packge_root(getwd(), quiet = TRUE)
  if (!is.null(x)) return(read.dcf(file.path(x, "DESCRIPTION")[[1L, "Package"]]))

  invisible(NULL)
}



package_desc <- function() {
  x <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA_character_)
  if (!is.na(x)) return(file.path(find.package(x), "DESCRIPTION"))

  x <- find_packge_root(getwd(), quiet = TRUE)
  if (!is.null(x)) return(file.path(x, "DESCRIPTION")[[1L, "Package"]])

  invisible(NULL)
}
