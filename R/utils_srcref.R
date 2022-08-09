srcref_key <- function(x, nloc = 2, path = c("base", "root", "full")) {
  path <- match.arg(path)

  stopifnot(nloc %in% c(2, 4, 6, 8))
  nloc_indxs <- list(c(1, 3), 1:4, 1:6, 1:8)
  nloc <- match(nloc, c(2, 4, 6, 8))
  nloc <- nloc_indxs[[nloc]]

  srcpath <- getSrcFilename(x, full.names = TRUE)
  pkgroot <- file.path(find_package_root(srcpath), "")

  srcpath <- switch(path,
    "full" = srcpath,
    "base" = basename(srcpath),
    "root" = gsub(pkgroot, "", srcpath)
  )

  sprintf(
    "%s:%s",
    srcpath,
    paste(as.numeric(getSrcref(x))[nloc], collapse = ":")
  )
}

as.srcref <- function(x) {
  UseMethod("as.srcref")
}

as.srcref.character <- function(x) {
  m <- regexpr("(?<filename>[^:]*):(?<location>.*)", x, perl = TRUE)
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


  location <- srclocs(as.numeric(strsplit(m[,"location"], ":")[[1]]), filename)
  srcref(srcfile(filename), location)
}

srclocs <- function(x, file) {
  if (length(x) < 4) {
    line <- x[[2]]
    x[[3]] <- x[[2]]
    x[[2]] <- 0
    x[[4]] <- if (file.exists(file)) file_line_nchar(file, line) else 0
  }
  x
}

file_line_nchar <- function(file, line) {
  nchar(scan(file, what = character(), skip = line - 1, n = 1, quiet = TRUE))
}
