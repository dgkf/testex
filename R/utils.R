`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}



`%|NA|%` <- function(lhs, rhs) {
  if (is.na(lhs)) rhs else lhs
}



#' Extract select unexported objects from a package namespace
#'
#' @param package A package name
#' @param names A \code{character} vector of object names to select
#'
priv <- function(package, names) {
  function() {
    if (requireNamespace(package, quietly = TRUE)) {
      ns <- as.list(getNamespace(package))
      if (any(!names %in% names(ns))) {
        stop(sprintf("required objects not found in %s", package))
      }
      ns[names]
    } else {
      message(sprintf("%s needed to use this functionality", package))
    }
  }
}



#' Private roxygen2 functions
.roxygen2 <- priv("roxygen2", c(
  "roclet_process.roclet_rd",
  "roclet_output.roclet_rd"
))



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



find_package_root <- function(path = ".", quiet = FALSE) {
  if (path == ".") path <- getwd()
  while (dirname(path) != path) {
    if (file.exists(file.path(path, "DESCRIPTION")))
      return(path)
    path <- dirname(path)
  }

  if (!quiet) stop("Unable to find package root")
  invisible(NULL)
}



package_name <- function() {
  x <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA_character_)
  if (!is.na(x)) return(x)

  x <- find_package_root(getwd(), quiet = TRUE)
  if (!is.null(x)) return(read.dcf(file.path(x, "DESCRIPTION")[[1L, "Package"]]))

  invisible(NULL)
}



package_desc <- function() {
  x <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA_character_)
  if (!is.na(x)) return(file.path(find.package(x), "DESCRIPTION"))

  x <- find_package_root(getwd(), quiet = TRUE)
  if (!is.null(x)) return(file.path(x, "DESCRIPTION")[[1L, "Package"]])

  invisible(NULL)
}


vlapply <- function(..., FUN.VALUE = logical(1L)) {
  vapply(..., FUN.VALUE = FUN.VALUE)
}

vcapply <- function(..., FUN.VALUE = character(1L)) {
  vapply(..., FUN.VALUE = FUN.VALUE)
}

vnapply <- function(..., FUN.VALUE = numeric(1L)) {
  vapply(..., FUN.VALUE = FUN.VALUE)
}
