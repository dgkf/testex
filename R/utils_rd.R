find_package_rds <- function(package, path = getwd()) {
  if (!missing(package)) {
    package_path <- find.package(package, quiet = TRUE)
    package_man <- file.path(package_path, "man")
    if (isTRUE(dir.exists(package_man))) rds <- tools::Rd_db(dir = package_path)
    else rds <- tools::Rd_db(package)
  } else {
    desc <- file.path(find_package_root(path), "DESCRIPTION")
    package <- read.dcf(desc, fields = "Package")[[1L]]
    rds <- tools::Rd_db(dir = path)
  }
}

rd_extract_examples <- function(rd) {
  rd_tags <- vapply(rd, attr, character(1L), "Rd_tag")
  rd_ex <- which(rd_tags == "\\examples")
  if (length(rd_ex) == 0L) return(NULL)
  rd[[rd_ex]]
}

rd_example_as_string <- function(rd) {
  paste(unlist(rd), collapse = "")
}

string_line_count <- function(x) {
  nchar(gsub("[^\n]", "", x))
}

split_srcref <- function(sr, where) {
  if (is.null(sr)) return(rep_len(sr, length(where)))
  file <- getSrcFilename(sr, full.names = TRUE)

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

split_testonly_as_expr <- function(rd) {
  rds <- split_testonly(rd)

  # convert from Rd tag list to code string
  code_segments <- lapply(rds, rd_example_as_string)
  code_segments_lines <- vnapply(code_segments, string_line_count)

  # filter out any unused lines
  segment_has_expr <- grepl("\\S", code_segments)
  code_segments <- code_segments[segment_has_expr]
  code_segments_lines <- code_segments_lines[segment_has_expr]
  code_exprs <- lapply(code_segments, str2lang)

  # split original srcref into srcrefs for individual expressions
  code_srcrefs <- split_srcref(getSrcref(rds), cumsum(code_segments_lines))
  for (i in seq_along(code_segments))
    attr(code_exprs[[i]], "srcref") <- code_srcrefs[[i]]

  code_exprs
}

split_testonly <- function(rd) {
  attrs <- attributes(rd)
  n <- length(rd)
  tags <- vapply(rd, attr, character(1L), "Rd_tag")
  is_test <- tags == "\\testonly"
  splits <- is_test
  splits[-1][is_test[-n]] <- TRUE
  splits <- cumsum(splits)
  res <- split(rd, splits)
  attributes(res) <- attrs
  names(res) <- vapply(res, function(i) attr(i[[1]], "Rd_tag"), character(1L))
  res
}
