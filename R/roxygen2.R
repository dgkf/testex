#' testex replacement for roxygen2 rd roclet
#'
#' This roclet aims to be feature compatible with \pkg{roxygen2}'s \code{"rd"}
#' roclet. In addition it supports two new \code{roxygen} tags, \code{@expect}
#' and \code{@testthat}.
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
  testex_tags <- c("expect", "testthat")
  rdname <- basename(base_path)
  obj_locs <- eapply(env, getSrcLocation, which = "line")

  for (bi in seq_along(blocks)) {
    block <- blocks[[bi]]
    tags <- vapply(block$tags, `[[`, character(1L), "tag")

    ti_ex_tag <- which(tags == "examples")
    ti_ex_last_subtag <- tail(which(tags %in% c("examples", testex_tags)), 1L)
    if (!length(ti_ex_tag)) break

    ex_tag <- block$tags[[ti_ex_tag]]
    ex <- ex_tag$val

    # try to track lines of code through example for test descriptions
    ex_file <- ex_tag$file
    ex_lines <- rep_len(ex_tag$line, 2L)

    # find next obj in source code
    obj <- names(obj_locs)[Position(function(i) i > ex_tag$line, obj_locs)]

    # stateful aggregators to collect consecutive tests into \testonly block
    last_tag <- NULL
    tests <- list()

    for (ti in seq(from = ti_ex_tag, to = ti_ex_last_subtag)) {
      tag <- block$tags[[ti]]

      if (!is.null(last_tag) && last_tag != tag$tag) {
        test_rd <- format_tests(
          last_tag,
          tests,
          obj = obj,
          file = ex_file,
          lines = ex_lines
        )

        ex <- append_test_rd(ex, test_rd)
        tests <- list()
      }

      switch(tag$tag,
        examples = next,
        expect =,
        testthat = {
          if (length(tests) == 0L) ex_lines[[2L]] <- tag$line - 1L
          tests <- append_test(tag$tag, tests, tag$test)

          if (nchar(trimws(tag$remainder)) > 0L) {
            testonly <- format_tests(
              last_tag %||% tag$tag,
              tests,
              obj = obj,
              file = ex_file,
              lines = ex_lines
            )
            remainder <- sub("^\n", "", tag$remainder)
            ex <- append_test_rd(ex, c(testonly, remainder))
            ex_lines <- rep_len(tag$line + srcref_nlines(tag$test), 2L)
            tests <- list()
          }
        },
        stop(paste0("Tag unexpected while building examples: ", tag$tag))
      )

      # now that we've merged the content into the example, flag for removal
      blocks[[bi]]$tags[[ti]] <- NULL
      last_tag <- tag$tag
    }

    if (!is.null(last_tag) && length(tests)) {
      test_rd <- format_tests(
        last_tag,
        tests,
        obj = obj,
        file = ex_file,
        lines = ex_lines
      )

      ex <- append_test_rd(ex, test_rd)
    }

    # filter out any tags that were merged into the example block
    blocks[[bi]]$tags <- Filter(Negate(is.null), blocks[[bi]]$tags)
    blocks[[bi]]$tags[[ti_ex_tag]]$val <- ex
  }

  .roxygen2()$roclet_process.roclet_rd(x, blocks, env, base_path)
}

#' @exportS3Method roxygen2::roclet_output roclet_rd
roclet_output.roclet_rd <- function(...) {
  .roxygen2()$roclet_output.roclet_rd(...)
}

#' @importFrom utils head tail
#' @exportS3Method roxygen2::roxy_tag_parse roxy_tag_expect
roxy_tag_parse.roxy_tag_expect <- function(x) {
  xlines <- strsplit(x$raw, "\n")[[1L]]

  # try to parse first expression
  status <- tryCatch({
      x$test <- parse(text = x$raw, n = 1, keep.source = TRUE)
      TRUE
    },
    error = function(e) e
  )

  # if parsing failed, use the error message to try to subset text before
  # parsing error and try to parse first expression again
  if (!isTRUE(status)) {
    msg <- conditionMessage(status)
    err_loc_re <- ":(\\d+):(\\d+):"
    m <- regexec(err_loc_re, msg)[[1L]]
    loc <- substring(msg, m[-1L], m[-1L] + attr(m, "match.length")[-1L] - 1L)
    loc <- as.numeric(loc)
    text <- head(xlines, loc[[1L]])
    text[[length(text)]] <- substring(tail(text, 1L), 1L, loc[[2L]] - 1L)

    status <- tryCatch({
        x$test <- parse(text = text, n = 1, keep.source = TRUE)
        TRUE
      },
      error = function(e) e
    )
  }

  if (!isTRUE(status)) {
    warning(
      "Error encountered while parsing expectation. This will likely ",
      "cause an error when testing examples."
    )
  }

  # update parsed lines with actual lines
  srcref <- attr(x$test, "srcref")[[1]]
  attr(srcref, "srcfile") <- srcfile(x$file)
  srcref[1] <- srcref[1] + x$line - 1L
  srcref[3] <- srcref[3] + x$line - 1L
  attr(x$test, "srcref") <- srcref

  # find coding test lines, to determine trailing raw lines
  test_lines <- as.character(attr(x$test, "srcref"), useSource = TRUE)
  test_loc <- c(length(test_lines), nchar(tail(test_lines, 1L)))

  x$remainder <- paste(collapse = "\n", c(
    substring(xlines[[test_loc[[1L]]]], test_loc[[2L]] + 1L),
    tail(xlines, -test_loc[[1L]])
  ))

  x
}

#' @exportS3Method roxygen2::roxy_tag_parse roxy_tag_testthat
roxy_tag_parse.roxy_tag_testthat <- roxy_tag_parse.roxy_tag_expect



#' Deparse an expression and indent for pretty-printing
#'
#' @param x A \code{code} object
#' @param indent An \code{integer} number of spaces or a string to prefix each
#'   line of the deparsed output.
#'
#' @family roclet_process_helpers
#'
deparse_indent <- function(x, indent = 0L) {
  if (is.numeric(indent)) indent <- strrep(" ", indent)
  paste0(indent, deparse(x), collapse = "\n")
}



#' Format test for an Rd \\testonly block
#'
#' @param tag The roxygen tag that we are formatting
#' @param tests A \code{list} of test \code{code} objects to be formatted into a
#'   \code{\\testonly} block.
#' @param ... Additional arguments used by methods
#'
#' @rdname format_tests
#' @family roclet_process_helpers
format_tests <- function(tag, tests, ...) {
  UseMethod("format_tests", structure(1L, class = tag))
}

#' @rdname format_tests
format_tests.expect <- function(tag, tests, obj, file, lines) {
  tests <- lapply(tests, `attributes<-`, NULL)
  tests <- vapply(tests, deparse_indent, character(1L), indent = 2L)

  example_src <- paste0(
    basename(file),
    ":", lines[[1]],
    ":", lines[[2]]
  )

  c(
    "\\testonly{",
    "testex::testex(",
      sprintf("%s,", escape_infotex(tests)),
      sprintf("  obj     = \"%s\",", obj),
      sprintf("  example = \"%s\"", example_src),
    ")}"
  )
}

#' @param file The source file where the example test code originated
#' @param lines A \code{numeric} vector of length two indicating the start and
#'   end lines of the example code block tested by the test code.
#'
#' @rdname format_tests
format_tests.testthat <- function(tag, tests, obj, file, lines) {
  srcs  <- vapply(tests, function(i) srcref_key(attr(i, "srcref"), nloc = 2L), character(1L))
  tests <- vapply(tests, deparse_indent, character(1L), indent = 2L)

  example_src <- paste0(
    basename(file),
    ":", lines[[1]],
    ":", lines[[2]]
  )

  desc <- sprintf(
    "`%s` example tests (%s)",
    obj,
    example_src
  )

  c(
    "\\testonly{",
      paste0("testex::testthat_block(test_that(", deparse(desc), ", {"),
      paste0("  testex::with_srcref(\"", srcs, "\", ", trimws(escape_infotex(tests)), ")"),
    "}),",
    sprintf("  obj     = \"%s\",", obj),
    sprintf("  example = \"%s\"", example_src),
    ")}"
  )
}



#' Restructure and append tests to a test aggregating list
#'
#' @param tag The roxygen tag that we are formatting
#' @param tests A \code{list} of test \code{code} objects to be formatted into a
#'   \code{\\testonly} block.
#' @param test A new test \code{code} object to append to the list. If
#'   necessary, the code will be modified to accommodate the testing style.
#'
#' @family roclet_process_helpers
append_test <- function(tag, tests, test) {
  UseMethod("append_test", structure(1L, class = tag))
}

append_test.expect <- function(tag, tests, test) {
  attrs <- attributes(test)
  test <- test[[1L]]
  if (!"." %in% all.names(test)) {
    test <- bquote(identical(., .(test)))
  }
  attributes(test) <- attrs
  append(tests, list(test))
}

append_test.testthat <- function(tag, tests, test) {
  attrs <- attributes(test)
  test <- test[[1L]]
  if (!"." %in% all.names(test))
    test <- as.call(append(as.list(test), quote(.), after = 1L))
  attributes(test) <- attrs
  append(tests, list(test))
}



#' Escape escaped Rd \\testonly strings
#'
#' @param x A \code{character} value
#'
#' @family roclet_process_helpers
escape_infotex <- function(x) {
  gsub("\\\\", "\\\\\\\\", x)
}



#' Determine the number of source code lines of a given srcref
#'
#' @param x A \code{srcref} object
#'
#' @family roclet_process_helpers
#'
#' @importFrom utils getSrcLocation
srcref_nlines <- function(x) {
  getSrcLocation(x, "line", first = FALSE) - getSrcLocation(x, "line") + 1L
}



#' Append a test to the examples Rd section
#'
#' @note
#' Because of how newlines are formatted when rendering Rd contents,
#' \code{\\testonly} blocks must starts on the last line of the code that they
#' test. Otherwise, an extra newline is printed when Rd is output as text.
#'
#' @param ex The existing character vector of example Rd section lines
#' @param test The additional test lines to add to the example
#'
#' @family roclet_process_helpers
append_test_rd <- function(ex, test) {
  c(
    ex[-length(ex)],
    # \testonly on same line to prevent unintended linebreaks
    paste(ex[[length(ex)]], test[[1L]]),
    test[-1L]
  )
}
