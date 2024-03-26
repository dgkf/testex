#' Rd Example Parsing Helpers
#'
#' @param rd An Rd object
#'
#' @name testex-rd-example-helpers
#' @keywords internal
#'
NULL



#' @describeIn testex-rd-example-helpers
#' Extract examples tag from an Rd file
#'
#' @return The examples section of an Rd object
#'
rd_extract_examples <- function(rd) {
  rd_tags <- vapply(rd, attr, character(1L), "Rd_tag")
  rd_ex <- which(rd_tags == "\\examples")
  if (length(rd_ex) == 0L) return(NULL)
  rd[[rd_ex]]
}



#' @describeIn testex-rd-example-helpers
#' Convert an Rd example to string
#'
#' @return A formatted Rd example
#'
rd_code_as_string <- function(rd) {
  if (inherits(rd, "\\dontrun"))
    paste(gsub("\\S", "", unlist(rd)), collapse = "")
  else
    paste(unlist(rd), collapse = "")
}



#' @describeIn testex-rd-example-helpers
#' Split sections of an example into evaluated example code blocks and code
#' blocks wrapped in `\testonly` `Rd_tag`s, reassigning [`srcref`]s as the 
#' example code is split.
#'
#' @return An interlaced list of expressions, either representing example
#'   code or tests. The names of the list are either `\testonly` or `RDCODE`
#'   depending on the originating source of the expression.
#'
split_testonly_as_expr <- function(rd) {
  rds <- split_testonly(rd)

  # convert Rd tag lists to strings (including \dontrun, converted to ws only)
  all_seg <- lapply(rds, rd_code_as_string)
  n <- length(all_seg)

  # resegment to combine any non-testonly sections
  resegment <- names(all_seg) == "\\testonly"
  resegment[-1] <- resegment[-n] | resegment[-1]
  resegment <- cumsum(resegment)
  code_seg <- split(all_seg, resegment)

  # preserver \testonly names, everything else can now be considered RCODE
  names(code_seg) <- names(all_seg)[!duplicated(resegment)]
  names(code_seg) <- ifelse(
    names(code_seg) == "\\testonly",
    "\\testonly",
    "RCODE"
  )

  code_seg <- lapply(code_seg, rd_code_as_string)
  code_seg_lines <- vnapply(code_seg, string_newline_count)

  # filter out any unused lines
  segment_has_expr <- grepl("\\S", code_seg)
  code_seg <- code_seg[segment_has_expr]
  code_seg_lines <- code_seg_lines[segment_has_expr]
  code_exprs <- lapply(code_seg, function(seg) {
    expr <- str2expression(seg)
    if (length(expr) == 1) expr[[1]]
    else as.call(append(list(as.symbol("{")), as.list(expr)))
  })

  # split original srcref into srcrefs for individual expressions
  code_srcrefs <- split_srcref(utils::getSrcref(rds), cumsum(code_seg_lines))
  for (i in seq_along(code_seg)) {
    attr(code_exprs[[i]], "srcref") <- code_srcrefs[[i]]
  }

  code_exprs
}



#' @describeIn testex-rd-example-helpers
#' Split sections of an example into lists of `Rd_tag`s. Note that [`srcref`]s
#' are split by line number. If a line is split between two sections, it is
#' attributed to the first section. As this is used primarily for giving line
#' numbers to test messages, this is sufficient for providing test failures
#' locations.
#'
#' @return A list of Rd tag contents
#'
split_testonly <- function(rd) {
  attrs <- attributes(rd)
  n <- length(rd)

  tags <- vapply(rd, attr, character(1L), "Rd_tag")
  is_cons <- logical(n)
  is_cons[-1] <- tags[-1] == tags[-n]
  cumsum(!is_cons)

  res <- split(rd, cumsum(!is_cons))
  split_tags <- tags[!is_cons]

  # rd tags of each split are applied as a subclass
  for (i in seq_along(res)) {
    class(res[[i]]) <- c(split_tags[[i]], class(res[[i]]))
  }

  attributes(res) <- attrs
  names(res) <- vapply(res, function(i) attr(i[[1]], "Rd_tag"), character(1L))
  res
}
