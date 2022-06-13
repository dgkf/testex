.testex_options <- new.env(parent = baseenv())



update_testex_desc <- function(path, fingerprint) {
  if (identical(fingerprint, .testex_options$.fingerprint)) {
    return(invisible(.testex_options))
  }

  field <- "Config/testex/options"
  desc_opts <- read.dcf(path, field, keep.white = field)[[1L]]
  desc_opts <- eval(parse(text = desc_opts), envir = baseenv())
  rm(list = names(.testex_options), envir = .testex_options)
  for (n in names(desc_opts)) .testex_options[[n]] <- desc_opts[[n]]
  .testex_options$.fingerprint <- fingerprint
  invisible(.testex_options)
}



testex_options <- function(path = package_desc()) {
  if (is_r_cmd_check()) {
    fingerprint <- list(rcmdcheck = TRUE, pid = Sys.getpid())
    return(as.list(update_testex_desc(path, fingerprint)))
  }

  if (file.exists(path)) {
    fingerprint <- list(desc = TRUE, path = path, mtime = file.info(path)[["mtime"]])
    return(as.list(update_testex_desc(path, fingerprint)))
  }

  return(as.list(.testex_options))
}
