#' Rd Example Parsing Helpers
#'
#' @param rd An Rd object
#'
#' @name testex-rd-example-helpers
#' @keywords internal
#'
NULL



#' @describeIn testex-rd-example-helpers
#'
#' Extract examples tag from an Rd file
#'
rd_extract_examples <- function(rd) {
  rd_tags <- vapply(rd, attr, character(1L), "Rd_tag")
  rd_ex <- which(rd_tags == "\\examples")
  if (length(rd_ex) == 0L) return(NULL)
  rd[[rd_ex]]
}



#' @describeIn testex-rd-example-helpers
#'
#' Convert an Rd example to string
#'
rd_example_as_string <- function(rd) {
  paste(unlist(rd), collapse = "")
}



#' @describeIn testex-rd-example-helpers
#'
#' Split sections of an example into evaluated example code blocks and code
#' blocks wrapped in testonly `Rd_tag`s, reassigning `srcref`s as the example
#' code is split.
#'
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
  code_srcrefs <- split_srcref(utils::getSrcref(rds), cumsum(code_segments_lines))
  for (i in seq_along(code_segments))
    attr(code_exprs[[i]], "srcref") <- code_srcrefs[[i]]

  code_exprs
}



#' @describeIn testex-rd-example-helpers
#'
#' Split sections of an example into lists of `Rd_tag`s
#'
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
