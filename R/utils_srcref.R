#' Convert a srcref to a character representation
#'
#' @param x A srcref object
#' @param nloc The number of src locations to use. Defaults to 2, indicating
#'   starting and ending line number.
#' @param path A form of filepath to use for the key. One of `"base"` for only
#'   the basename of the source filepath, `"root"` for a path relative to a
#'   package root directory if found, or `"full"` for the full filepath.
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
  if (!length(pkgroot)) pkgroot <- ""
  else pkgroot <- paste0(pkgroot, .Platform$file.sep)

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



#' Convert to srcref
#'
#' @param x an object to coerce
#'
#' @name as.srcref
#' @keywords internal
as.srcref <- function(x) {
  UseMethod("as.srcref")
}



#' @describeIn as.srcref
#'
#' Convert from a `srcref_key` to a sourceref object
#'
as.srcref.character <- function(x) {
  m <- regexpr("(?<filename>.*?)(?<location>(:\\d+)+)", x, perl = TRUE)
  m <- matrix(
    substring(x, s <- attr(m, "capture.start"), s + attr(m, "capture.length") - 1),
    nrow = length(x),
    dimnames = list(x, colnames(s))
  )

  filename <- m[,"filename"]
  pkgroot <- tryCatch(find_package_root(), error = function(e) NULL)

  if (!is.null(pkgroot)) {
    if (file.exists(f <- file.path(pkgroot, filename))) filename <- f
    if (file.exists(f <- file.path(pkgroot, "R", filename))) filename <- f
  }

  location <- srclocs(as.numeric(strsplit(m[,"location"], ":")[[1]][-1]), filename)
  srcref(srcfile(filename), location)
}



#' Build srcLocation from a minimal numeric vector
#'
#' Build a length four source location from a length two source location. The
#' starting column on the first line is assumed to be 1, and the final column is
#' taken to be the length of the line if the source file exists, or 1 as a
#' fallback.
#'
#' @param x A numeric vector of at least length 2
#' @param file A file to use to determine the length of the final line
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



#' Split a srcref into separate srcrefs at specific lines
#'
#' @param sr An original srcref object
#' @param where A numeric vector of line numbers where the srcref should be
#'   split
#'
#' @importFrom utils getSrcFilename
#' @keywords internal
split_srcref <- function(sr, where) {
  if (is.null(sr)) return(rep_len(sr, length(where)))
  file <- utils::getSrcFilename(sr, full.names = TRUE)

  # allocate a list of new srcrefs
  refs <- list()
  length(refs) <- length(where)

  # starting from start of collective srcref, offset local lines
  start <- getSrcLocation(sr)
  where <- start + where

  # create new srcrefs of regions, divided by "where" lines
  for (i in seq_along(where)) {
    locs <- srclocs(c(start, where[[i]]), file)
    refs[[i]] <- srcref(srcfile(file), locs)
    start <- where[[i]] + 1
  }

  refs
}



#' Determine the number of source code lines of a given srcref
#'
#' @param x A `srcref` object
#'
#' @importFrom utils getSrcLocation
#' @keywords internal
srcref_nlines <- function(x) {
  getSrcLocation(x, "line", first = FALSE) - getSrcLocation(x, "line") + 1L
}
