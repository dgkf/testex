.testex_options <- new.env(parent = baseenv())



#' Cached retrieval of testex options from package DESCRIPTION
#'
#' As long as the `fingerprint` has not changed, the package `DESCRIPTION` will
#' be read only once to parse and retrieve configuration options. If the
#' `DESCRIPTION` file is modified or if run from a separate process, the
#' configured settings will be refreshed based on the most recent version of
#' the file.
#'
#' @param path A path in which to search for a package `DESCRIPTION`
#' @param fingerprint An object used to indicate when the cached values have
#'   been invalidated
#'
#' @return The test options environment, invisibly.
#'
#' @name testex-options
#' @keywords internal
memoise_testex_desc <- function(path, fingerprint, ...) {
  if (identical(fingerprint, .testex_options$.fingerprint)) {
    return(invisible(.testex_options))
  }

  desc_opts <- read_testex_options(path, ...)

  # clean and re-load memoised options
  rm(list = names(.testex_options), envir = .testex_options)
  for (n in names(desc_opts)) .testex_options[[n]] <- desc_opts[[n]]

  .testex_options$.fingerprint <- fingerprint
  invisible(.testex_options)
}



read_testex_options <- function(path, warn = TRUE, update = FALSE) {
  desc <- read.dcf(file = path, all = TRUE)
  desc <- read.dcf(file = path, keep.white = colnames(desc))

  field <- "Config/testex/options"
  desc_opts <- if (field %in% colnames(desc)) desc[, field][[1]] else ""

  # the field name is erroneously parsed with the contents on R <4.1 in CMD check
  desc_opts <- gsub(paste0(field, ": "), "", desc_opts, fixed = TRUE)
  pkg_opts <- pkg_opts_orig <- eval(parse(text = desc_opts), envir = baseenv())
  loaded_version <- packageVersion(packageName())
  loaded_version_str <- as.character(loaded_version)

  warn_mismatch_msg <- cliless(
    "{.pkg testex} {.code version} in {.file DESCRIPTION} does not match ",
    "currently loaded version. Consider updating to avoid unexpected test ",
    "failures. Execution during {.code R CMD check} disabled."
  )

  if (update) {
    # update registered version if necessary
    if (is.null(pkg_opts$version) || pkg_opts$version < loaded_version) {
      pkg_opts$version <- loaded_version_str
    }

    # only write if field was modified
    if (!identical(pkg_opts, pkg_opts_orig)) {
      if (!field %in% colnames(desc)) {
        field_col <- matrix(nrow = nrow(desc), dimnames = list(c(), field))
        desc <- cbind(desc, field_col)
      }

      desc[, field] <- deparse(pkg_opts)

      write.dcf(
        desc,
        file = path,
        keep.white = colnames(desc),
        width = 80L,
        indent = 2L
      )
    }
  }

  if (!identical(pkg_opts$version, loaded_version_str)) {
    if (warn) warning(warn_mismatch_msg)
    pkg_opts$check <- FALSE
  }

  pkg_opts
}



#' @describeIn testex-options
#'
#' @return The test options environment as a list
#'
testex_options <- function(path = package_desc(), ...) {
  path <- package_desc(path)

  if (is_r_cmd_check()) {
    fingerprint <- list(rcmdcheck = TRUE, pid = Sys.getpid())

    # don't warn or update description during checking
    return(as.list(memoise_testex_desc(
      path,
      fingerprint,
      warn = FALSE,
      update = FALSE
    )))
  }

  if (!is.null(path) && file.exists(path)) {
    fingerprint <- list(
      desc = TRUE,
      path = path,
      mtime = file.info(path)[["mtime"]]
    )

    return(as.list(memoise_testex_desc(path, fingerprint, ...)))
  }

  return(as.list(.testex_options))
}
