#' testex replacement for roxygen2 rd roclet
#'
#' This roclet aims to be feature compatible with \pkg{roxygen2}'s \code{"rd"}
#' roclet. In addition it supports two new \code{roxygen} tags, \code{@expect}
#' and \code{@testthat}.
#'
#' @return A new `roxygen2` `"rd"` roclet.
#'
#' @section tags:
#' \code{testex} tags are all sub-tags meant to be used within an
#' \code{@examples} block. They should be considered as tags \emph{within} the
#' \code{@examples} block and used to construct blocks of testing code within
#' example code.
#'
#' \describe{
#'   \item{\code{@expect}: }{
#'     In-line expectations to test the output of the previous command within an
#'     example. If \code{.} is used within the expecation, it will be used to
#'     refer to the output of the previous example command. Otherwise, the
#'     result of the expression is expected to be identical to the previous
#'     output.
#'   }
#' }
#'
#' \preformatted{
#' #' @examples
#' #' 1 + 2
#' #' @expect 3
#' #' @expect . == 3
#' #'
#' #' 3 + 4
#' #' @expect identical(., 7)
#' }
#'
#' \describe{
#'   \item{\code{@testthat}: }{
#'     Similar to \code{@expect}, \code{@testthat} can be used to make in-line
#'     assertions using \pkg{testthat} expectations. \pkg{testthat} expectations
#'     follow a convention where the first argument is an object to compare
#'     against an expected value or characteristic. Since the value will always
#'     be the result of the previous example, this part of the code is
#'     implicitly constructed for you.
#'
#'     If you want to use the example result elsewhere in your expectation, you
#'     can refer to it with a \code{.}. When used in this way, \pkg{testex} will
#'     not do any further implicit modification of your expectation.
#'   }
#' }
#'
#' \preformatted{
#' #' @examples
#' #' 1 + 2
#' #' @testthat expect_equal(3)
#' #' @testthat expect_gt(0)
#' #'
#' #' 3 + 4
#' #' @testthat expect_equal(., 7)
#' }
#'
#' @export
rd <- function() {
  roxygen2::roclet("rd")
}

#' @importFrom utils tail
#' @exportS3Method roxygen2::roclet_process roclet_rd
roclet_process.roclet_rd <- function(x, blocks, env, base_path) {
  for (bi in seq_along(blocks)) {
    blocks[[bi]] <- roclet_process_testex(blocks[[bi]])
  }

  .roxygen2()$roclet_process.roclet_rd(x, blocks, env, base_path)
}

roclet_process_testex <- function(block) {
  testex_tags <- c("expect", "testthat")

  idx_ex_tag <- roclet_which_example_tag(block$tags)
  if (!length(idx_ex_tag)) return(block)

  # initial example, to which we'll merge in formatted tests code
  ex_tag <- block$tags[[idx_ex_tag]]
  ex <- ex_tag$val

  # aggregate expectations and srcref line ranges for consecutive test tags
  expsloc <- rep_len(ex_tag$line, 2L)
  exps <- list()

  i <- idx_ex_tag + 1
  while (i <= length(block$tags)) {
    # read next tag, splitting content into example code and test code
    tag <- block$tags[[i]]
    if (!tag$tag %in% testex_tags) break

    # update expectation line range to include next tag
    if (length(exps) == 0L) expsloc[[2L]] <- tag$line - 1L
    exps <- append_test(tag, exps)

    # flush expects if back to example code (remainder) or last tag of test block
    is_last <- !isTRUE(tag$tag == block$tags[[i + 1]]$tag)
    if (roxy_test_has_remainder(tag) || is_last) {
      rd <- format_tests(tag$tag, exps, file = block$file, lines = expsloc)
      ex <- append_test_rd(ex, c(rd, sub("^\n", "", tag$remainder)))
      expsloc <- rep_len(tag$line + !is_last * srcref_nlines(tag$test), 2L)
      exps <- list()
    }

    # strip original tag from block
    block$tags[i] <- list(NULL)
    i <- i + 1
  }

  # filter out any tags that were merged into the example block
  block$tags[[idx_ex_tag]]$val <- ex
  block$tags <- Filter(Negate(is.null), block$tags)

  block
}

roclet_which_example_tag <- function(tags) {
  tags <- vcapply(tags, `[[`, "tag")
  which(tags == "examples")
}

#' @exportS3Method roxygen2::roclet_output roclet_rd
roclet_output.roclet_rd <- function(...) {
  .roxygen2()$roclet_output.roclet_rd(...)
}

#' @importFrom utils head tail
#' @exportS3Method roxygen2::roxy_tag_parse roxy_tag_expect
roxy_tag_parse.roxy_tag_expect <- function(x) {
  x$test <- roxy_test_try_parse(x$raw)
  if (inherits(x$test, "error")) {
    warning(
      "Error encountered while parsing expectation. This will likely ",
      "cause an error when testing examples."
    )
  }

  x$test <- roxy_test_update_srcref(
    x$test,
    file = x$file,
    offset_lines = x$line - 1L
  )

  x$remainder <- roxy_test_raw_remainder(x)

  x
}

#' @exportS3Method roxygen2::roxy_tag_parse roxy_tag_testthat
roxy_tag_parse.roxy_tag_testthat <- roxy_tag_parse.roxy_tag_expect

roxy_test_try_parse <- function(x) {
  # try to parse first expression
  res <- tryCatch(
    parse(text = x, n = 1, keep.source = TRUE),
    error = function(e) e
  )

  # if parsing failed, use the error message to try to subset text before
  # parsing error and try to parse first expression again
  if (inherits(res, "error")) {
    msg <- conditionMessage(res)
    err_loc_re <- ":(\\d+):(\\d+):"
    m <- regexec(err_loc_re, msg)[[1L]]
    loc <- substring(msg, m[-1L], m[-1L] + attr(m, "match.length")[-1L] - 1L)
    loc <- as.numeric(loc)
    text <- head(strsplit(x, "\n")[[1L]], loc[[1L]])
    text[[length(text)]] <- substring(tail(text, 1L), 1L, loc[[2L]] - 1L)

    res <- tryCatch(
      parse(text = text, n = 1, keep.source = TRUE),
      error = function(e) e
    )
  }

  res
}

roxy_test_has_remainder <- function(tag) {
  nchar(trimws(tag$remainder)) > 0
}

roxy_test_update_srcref <- function(x, file, offset_lines) {
  # update parsed lines with actual lines
  srcref <- attr(x, "srcref")[[1]]

  if (file.exists(file))
    attr(srcref, "srcfile") <- srcfile(file)

  srcref[1] <- srcref[1] + offset_lines
  srcref[3] <- srcref[3] + offset_lines
  srcref[4] <- file_line_nchar(file, srcref[3])
  attr(x, "srcref") <- srcref
  x
}



#' Separate the remaining text following a parsed roxygen test
#'
#' @param x A processed roxy testex tag, including a `$test` field
#' @return the string that follows the end of the test expression
#'
#' @name roxy_test_helpers
#' @keywords internal
roxy_test_raw_remainder <- function(x) {
  xlines <- strsplit(x$raw, "\n")[[1L]]

  # find coding test lines, to determine trailing raw lines
  test_lines <- as.character(attr(x$test, "srcref"), useSource = TRUE)
  test_loc <- c(length(test_lines), nchar(tail(test_lines, 1L)))

  paste(collapse = "\n", c(
    substring(xlines[[test_loc[[1L]]]], test_loc[[2L]] + 1L),
    tail(xlines, -test_loc[[1L]])
  ))
}



#' Format test for an Rd \\testonly block
#'
#' @param tag The roxygen tag that we are formatting
#' @param tests A \code{list} of test \code{code} objects to be formatted into a
#'   \code{\\testonly} block.
#' @param ... Additional arguments used by methods
#'
#' @return A formatted block of code for an Rd section
#'
#' @rdname format_tests
#' @family roclet_process_helpers
#' @keywords internal
format_tests <- function(tag, tests, ...) {
  if (!length(tests)) return(character(0L))
  UseMethod("format_tests", structure(1L, class = tag))
}

#' @rdname format_tests
format_tests.default <- function(tag, tests, file, lines) {
  character(0L)
}

#' @rdname format_tests
format_tests.expect <- function(tag, tests, file, lines) {
  tests <- lapply(tests, `attributes<-`, NULL)
  tests <- vcapply(tests, deparse_indent, indent = 2L)
  example_src <- paste0(basename(file), ":", lines[[1]], ":", lines[[2]])

  c(
    "\\testonly{",
    "testex::testex(",
      sprintf("%s,", escape_infotex(tests)),
      sprintf("  example = \"%s\"", example_src),
    ")}"
  )
}

#' @param file The source file where the example test code originated
#' @param lines A \code{numeric} vector of length two indicating the start and
#'   end lines of the example code block tested by the test code.
#'
#' @rdname format_tests
format_tests.testthat <- function(tag, tests, file, lines) {
  srcs  <- vcapply(tests, function(i) srcref_key(attr(i, "srcref"), nloc = 2L))
  tests <- vapply(tests, deparse_indent, character(1L), indent = 2L)
  example_src <- paste0(basename(file), ":", lines[[1]], ":", lines[[2]])
  desc <- sprintf("example tests at `%s`", example_src)

  c(
    "\\testonly{",
      paste0("testex::testthat_block(test_that(", deparse(desc), ", {"),
      paste0("  testex::with_srcref(\"", srcs, "\", ", trimws(escape_infotex(tests)), ")"),
    "}),",
    sprintf("  example = \"%s\"", example_src),
    ")}"
  )
}



#' Restructure and append tests to a test aggregating list
#'
#' @param tag The roxygen tag that we are formatting
#' @param tests A `list` of test `code` objects to be formatted into a
#'   `\testonly` block.
#' @param test A new test `code` object to append to the list. If
#'   necessary, the code will be modified to accommodate the testing style.
#'
#' @return An appended `tests` `list`
#'
#' @family roclet_process_helpers
#' @keywords internal
append_test <- function(tag, tests) {
  UseMethod("append_test", structure(1L, class = tag$tag))
}

append_test.expect <- function(tag, tests) {
  attrs <- attributes(tag$test)
  test <- tag$test[[1L]]
  if (!"." %in% all.names(test)) {
    test <- bquote(identical(., .(test)))
  }
  attributes(test) <- attrs
  append(tests, list(test))
}

append_test.testthat <- function(tag, tests) {
  attrs <- attributes(tag$test)
  test <- tag$test[[1L]]
  if (!"." %in% all.names(test))
    test <- as.call(append(as.list(test), quote(.), after = 1L))
  attributes(test) <- attrs
  append(tests, list(test))
}



#' Escape escaped Rd \\testonly strings
#'
#' @param x A \code{character} value
#'
#' @return An escaped string, where any `\` is converted to `\\`
#'
#' @family roclet_process_helpers
#' @keywords internal
escape_infotex <- function(x) {
  gsub("\\\\", "\\\\\\\\", x)
}



#' Append a test to the examples Rd section
#'
#' @note
#' Because of how newlines are formatted when rendering Rd contents,
#' `\testonly` blocks must starts on the last line of the code that they
#' test. Otherwise, an extra newline is printed when Rd is output as text.
#'
#' @param ex The existing character vector of example Rd section lines
#' @param test The additional test lines to add to the example
#'
#' @return The result of concatenating `test` into `ex`
#'
#' @family roclet_process_helpers
#' @keywords internal
append_test_rd <- function(ex, test) {
  if (!length(test)) return(ex)
  c(
    ex[-length(ex)],
    # \testonly on same line to prevent unintended linebreaks
    paste0(ex[[length(ex)]], test[[1L]]),
    test[-1L]
  )
}
