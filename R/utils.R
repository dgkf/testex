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
#' @name testex-private-imports
#' @keywords internal
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
#'
#' @name testex-private-imports
#' @keywords internal
#'
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
  nsname <- if (isNamespace(ns)) getNamespaceName(ns) else ns
  if (paste0("package:", nsname) %in% search()) return(eval(expr))

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



#' Test whether currently executing R CMD check
#'
#' @keywords internal
is_r_cmd_check <- function() {
  !is.na(Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA_character_))
}



#' Package source file helpers
#'
#' Discover specific package related file paths
#'
#' @param path A path within a package source or install directory
#' @param quiet Whether to suppress output
#'
#' @name package-file-helpers
#' @keywords internal
#'
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



#' Find and return a package's Rd db
#'
#' @param package A package name
#' @param path A file path within a package's source code or installation
#'   directory. Only considered if `package` is missing.
#'
#' @name package-file-helpers
find_package_rds <- function(package, path = getwd()) {
  if (!missing(package)) {
    package_path <- find.package(package, quiet = TRUE)
  } else {
    package_path <- find_package_root(path)
  }

  desc <- file.path(package_path, "DESCRIPTION")
  package <- read.dcf(desc, fields = "Package")[[1L]]

  has_R_dir <- isTRUE(dir.exists(file.path(package_path, "R")))
  has_Meta_dir <- isTRUE(dir.exists(file.path(package_path, "Meta")))

  if (has_R_dir && !has_Meta_dir) {
    return(tools::Rd_db(dir = package_path))
  }

  if (has_Meta_dir) {
    return(tools::Rd_db(package = package, lib.loc = dirname(package_path)))
  }

  tools::Rd_db(package)
}



#' @name package-file-helpers
package_desc <- function() {
  x <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA_character_)
  if (!is.na(x)) return(file.path(find.package(x), "DESCRIPTION"))

  x <- find_package_root(getwd(), quiet = TRUE)
  if (!is.null(x)) return(file.path(x, "DESCRIPTION"))

  invisible(NULL)
}



#' `vapply` shorthands
#'
#' Simple wrappers around `vapply` for common data types
#'
#' @rdname vapplys
#' @inheritParams base::vapply
#' @keywords internal
vlapply <- function(..., FUN.VALUE = logical(1L)) {
  vapply(..., FUN.VALUE = FUN.VALUE)
}

#' @rdname vapplys
vcapply <- function(..., FUN.VALUE = character(1L)) {
  vapply(..., FUN.VALUE = FUN.VALUE)
}

#' @rdname vapplys
vnapply <- function(..., FUN.VALUE = numeric(1L)) {
  vapply(..., FUN.VALUE = FUN.VALUE)
}



#' Deparse pretty
#'
#' Deparse to a single string with two spaces of indentation
#'
#' @param expr An expression to deparse
#'
#' @keywords internal
deparse_pretty <- function(expr) {
  lines <- deparse(expr, width.cutoff = 120L)
  paste0(gsub("^(  +)\\1", "\\1", lines), collapse = "\n")
}



#' Deparse an expression and indent for pretty-printing
#'
#' @param x A \code{code} object
#' @param indent An \code{integer} number of spaces or a string to prefix each
#'   line of the deparsed output.
#'
#' @keywords internal
deparse_indent <- function(x, indent = 0L) {
  if (is.numeric(indent)) indent <- strrep(" ", indent)
  paste0(indent, deparse(x), collapse = "\n")
}



#' Return the number of lines in a multi-line string
#'
#' @param x A character value
#'
#' @keywords internal
string_line_count <- function(x) {
  nchar(gsub("[^\n]", "", x))
}



#' Return the number of characters in a line of a file
#'
#' @param file A file to use as reference
#' @param line A line number to retrieve the length of
#'
#' @keywords internal
file_line_nchar <- function(file, line) {
  if (!file.exists(file) || isTRUE(grepl("^<.*>$", basename(file)))) return(10000)
  nchar(scan(file, what = character(), skip = line - 1, n = 1, sep = "\n", quiet = TRUE))
}
