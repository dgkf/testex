#' Convert a [`srcref`] to a [`character`] representation
#'
#' @param x A [`srcref`] object
#' @param nloc The number of locations ([`utils::getSrcLocation`]) to use.
#'   Defaults to 2, indicating starting and ending line number.
#' @param path A form of file path to use for the key. One of `"base"` for only
#'   the basename of the source file path, `"root"` for a path relative to a
#'   package root directory if found, or `"full"` for the full file path.
#'
#' @return A string hash of a [srcref]
#'
#' @keywords internal
#' @importFrom utils getSrcref getSrcFilename
srcref_key <- function(x, nloc = 2, path = c("base", "root", "full")) {
  path <- match.arg(path)

  stopifnot(nloc %in% c(2, 4, 6, 8))
  nloc_indxs <- list(c(1, 3), 1:4, 1:6, 1:8)
  nloc <- match(nloc, c(2, 4, 6, 8))
  nloc <- nloc_indxs[[nloc]]
  loc <- paste(as.numeric(utils::getSrcref(x))[nloc], collapse = ":")

  srcpath <- utils::getSrcFilename(x, full.names = TRUE)
  pkgroot <- find_package_root(srcpath, quiet = TRUE)
  if (!length(pkgroot)) {
    pkgroot <- ""
  } else {
    pkgroot <- paste0(pkgroot, .Platform$file.sep)
  }

  srcpath <- switch(path,
    "full" = srcpath,
    "base" = basename(srcpath),
    "root" = {
      if (isTRUE(startsWith(srcpath, pkgroot))) {
        substring(srcpath, nchar(pkgroot) + 1)
      } else {
        srcpath
      }
    }
  )

  paste0(srcpath, ":", loc)
}



#' Convert to [srcref]
#'
#' @param x an object to coerce
#' @return A [srcref]
#'
#' @name as.srcref
#' @keywords internal
as.srcref <- function(x) {
  UseMethod("as.srcref")
}



#' @describeIn as.srcref
#' Convert from a `srcref_key` to a [srcref] object
#'
as.srcref.character <- function(x) {
  m <- regexpr("(?<filename>.*?)(?<location>(:\\d+)+)", x, perl = TRUE)
  m <- matrix(
    substring(x, s <- attr(m, "capture.start"), s + attr(m, "capture.length") - 1),
    nrow = length(x),
    dimnames = list(x, colnames(s))
  )

  filename <- m[, "filename"]
  pkgroot <- find_package_root(quiet = TRUE)

  filepath <- if (is.null(pkgroot)) {
    filename
  } else if (file.exists(f <- file.path(pkgroot, filename))) {
    f
  } else if (file.exists(f <- file.path(pkgroot, "R", filename))) {
    f
  } else {
    filename
  }

  location <- srclocs(
    as.numeric(strsplit(m[, "location"], ":")[[1]][-1]),
    filename
  )

  src_file <- srcfilealias(filename, srcfile(filepath))
  srcref(src_file, location)
}



#' Build a source location from a minimal numeric vector
#'
#' Build a length four source location from a length two source location. The
#' starting column on the first line is assumed to be 1, and the final column is
#' taken to be the length of the line if the source file exists, or 1 as a
#' fallback.
#'
#' @param x A numeric vector of at least length 2
#' @param file A file to use to determine the length of the final line
#'
#' @return A numeric vector similar to a [`utils::getSrcLocation`] object
#'
#' @keywords internal
srclocs <- function(x, file) {
  if (length(x) < 4) {
    line <- x[[2]]
    x[[3]] <- x[[2]]
    x[[2]] <- 1
    x[[4]] <- if (file.exists(file)) file_line_nchar(file, line) else 1
  }
  x
}



#' Split a Source Reference at specific lines
#'
#' @param sr An original [srcref] object
#' @param where A numeric vector of line numbers where the [srcref] should be
#'   split
#'
#' @return A list of [srcref]
#'
#' @importFrom utils getSrcFilename
#' @keywords internal
split_srcref <- function(sr, where) {
  if (is.null(sr)) {
    return(rep_len(sr, length(where)))
  }
  file <- utils::getSrcFilename(sr, full.names = TRUE)

  # allocate a list of new [srcref]s
  refs <- list()
  length(refs) <- length(where)

  # starting from start of collective srcref, offset local lines
  start <- getSrcLocation(sr)
  where <- start + where

  # create new [srcref]s of regions, divided by "where" lines
  for (i in seq_along(where)) {
    locs <- srclocs(c(start, where[[i]]), file)
    refs[[i]] <- srcref(srcfile(file), locs)
    start <- where[[i]] + 1
  }

  refs
}



#' Determine the number of source code lines of a given [srcref]
#'
#' @param x A [srcref] object
#' @return The number of lines in the original source of a [srcref]
#'
#' @importFrom utils getSrcLocation
#' @keywords internal
srcref_nlines <- function(x) {
  getSrcLocation(x, "line", first = FALSE) - getSrcLocation(x, "line") + 1L
}
