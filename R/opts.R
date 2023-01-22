.testex_options <- new.env(parent = baseenv())



#' Cached retrieval of testex options from package DESCRIPTION
#'
#' As long as the `fingerprint` has not changed, the package `DESCRIPTION` will
#' be read only once to parse and retrieve configuration options. If the
#' `DESCRIPTION` file is modified or if run from a separate process, the config
#' will be refreshed based on the most recent version of the file.
#'
#' @param path A path in which to search for a package `DESCRIPTION`
#' @param fingerprint An object used to indicate when the cached values have
#'   been invalidated
#'
#' @return The test options environment, invisibly.
#'
#' @name testex-options
#' @keywords internal
update_testex_desc <- function(path, fingerprint) {
  if (identical(fingerprint, .testex_options$.fingerprint)) {
    return(invisible(.testex_options))
  }

  field <- "Config/testex/options"
  desc_opts <- read.dcf(file = path, fields = field, keep.white = field)[[1L]]

  # the field name is erroneously parsed with the contents on R <4.1 in CMD check
  desc_opts <- gsub(paste0(field, ": "), "", desc_opts, fixed = TRUE)

  desc_opts <- eval(parse(text = desc_opts), envir = baseenv())
  rm(list = names(.testex_options), envir = .testex_options)
  for (n in names(desc_opts)) .testex_options[[n]] <- desc_opts[[n]]
  .testex_options$.fingerprint <- fingerprint
  invisible(.testex_options)
}



#' @describeIn testex-options
#'
#' @return The test options environemnt as a list
#'
testex_options <- function(path = package_desc()) {
  if (is_r_cmd_check()) {
    fingerprint <- list(
      rcmdcheck = TRUE,
      pid = Sys.getpid()
    )

    return(as.list(update_testex_desc(path, fingerprint)))
  }

  if (file.exists(path)) {
    fingerprint <- list(
      desc = TRUE,
      path = path,
      mtime = file.info(path)[["mtime"]]
    )

    return(as.list(update_testex_desc(path, fingerprint)))
  }

  return(as.list(.testex_options))
}
