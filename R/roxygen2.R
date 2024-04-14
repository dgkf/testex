#' [`testex`] `roxygen2` tags
#'
#' [`testex`] provides two new `roxygen2` tags, `@test` and `@testthat`.
#'
#' @section tags:
#' [testex] tags are all sub-tags meant to be used within an
#' `@examples` block. They should be considered as tags \emph{within} the
#' `@examples` block and used to construct blocks of testing code within
#' example code.
#'
#' \describe{
#'   \item{`@test`: }{
#' In-line expectations to test the output of the previous command within an
#' example. If `.` is used within the test expression, it will be used to
#' refer to the output of the previous example command. Otherwise, the
#' result of the expression is expected to be identical to the previous
#' output.
#'
#'     #' @examples
#'     #' 1 + 2
#'     #' @test 3
#'     #' @test . == 3
#'     #'
#'     #' @examples
#'     #' 3 + 4
#'     #' @test identical(., 7)
#'   }
#' }
#'
#' \describe{
#'   \item{`@testthat`: }{
#' Similar to `@test`, `@testthat` can be used to make in-line
#' assertions using `testthat` expectations. `testthat` expectations
#' follow a convention where the first argument is an object to compare
#' against an expected value or characteristic. Since the value will always
#' be the result of the previous example, this part of the code is
#' implicitly constructed for you.
#'
#' If you want to use the example result elsewhere in your expectation, you
#' can refer to it with a `.`. When used in this way, [testex] will
#' not do any further implicit modification of your expectation.
#'
#'     #' @examples
#'     #' 1 + 2
#'     #' @testthat expect_equal(3)
#'     #' @testthat expect_gt(0)
#'     #'
#'     #' @examples
#'     #' 3 + 4
#'     #' @testthat expect_equal(., 7)
#'   }
#' }
#'
#' @name testex-roxygen-tags
NULL



#' @importFrom utils head tail
#' @exportS3Method roxygen2::roxy_tag_parse roxy_tag_test
roxy_tag_parse.roxy_tag_test <- function(x) {
  testex_options(path = x$file, warn = TRUE, update = TRUE)
  x$raw <- x$val <- format_tag_expect_test(x)
  as_example(x)
}

#' @importFrom utils head tail
#' @exportS3Method roxygen2::roxy_tag_parse roxy_tag_test
roxy_tag_parse.roxy_tag_testthat <- function(x) {
  testex_options(path = x$file, warn = TRUE, update = TRUE)
  x$raw <- x$val <- format_tag_testthat_test(x)
  as_example(x)
}



#' Convert a `roxygen2` Tag to an `@examples` Tag
#'
#' Allows for converting testing tags into additional `@examples` tags, which
#' `roxygen2` will joint together into a single examples section.
#'
#' @param tag A `roxygen2` tag, whose class should be converted into an
#'   `@examples` tag.
#' @return The tag with an appropriate examples s3 class.
#'
#' @noRd
#' @keywords internal
as_example <- function(tag) {
  class(tag) <- class(tag)[!startsWith(class(tag), "roxy_tag_")]
  class(tag) <- c("roxy_tag_examples", class(tag))
  roxygen2::tag_examples(tag)
}



#' Format An `@test` Tag
#'
#' @param tag A `roxygen2` `@test` tag.
#' @return A formatted string of R documentation `\testonly{}` code.
#'
#' @noRd
#' @keywords internal
format_tag_expect_test <- function(tag) { # nolint
  parsed_test <- parse(text = tag$raw, n = 1, keep.source = TRUE)
  test <- populate_test_dot(parsed_test)
  n <- first_expr_end(parsed_test)

  test_str <- trimws(substring(tag$raw, 0, n), "right")
  n_newlines <- nchar(gsub("[^\n]", "", test_str))

  srcref_str <- paste0(
    basename(tag$file),
    ":", tag$line, ":", tag$line + n_newlines
  )

  paste0(
    "\\testonly{\n",
    "testex::testex(srcref = ", deparse(srcref_str), ", \n",
    deparse_pretty(test),
    ")}",
    trimws(substring(tag$raw, n + 1L), "right")
  )
}

#' Populate An Implicit `@test` Lambda Function
#'
#' When a `@test` tag does not contain a `.` object, its result is considered
#' an an implicit test for an identical object.
#'
#' @param expr A (possibly) implicity lambda function
#' @return A new expression, calling identical if needed.
#'
#' @noRd
#' @keywords internal
populate_test_dot <- function(expr) {
  if (is.expression(expr)) expr <- expr[[1]]
  if (!"." %in% all.names(expr)) {
    expr <- bquote(identical(., .(expr)))
  }
  expr
}



#' Format An `@testthat` Tag
#'
#' @param tag A `roxygen2` `@testthat` tag.
#' @return A formatted string of R documentation `\testonly{}` code.
#'
#' @noRd
#' @keywords internal
format_tag_testthat_test <- function(tag) { # nolint
  parsed_test <- parse(text = tag$raw, n = 1, keep.source = TRUE)
  test <- populate_testthat_dot(parsed_test)

  n <- first_expr_end(parsed_test)
  test_str <- substring(tag$raw, 1L, n)

  nlines <- string_newline_count(trimws(test_str, "right"))
  lines <- tag$line + c(0L, nlines)
  src <- paste0(basename(tag$file), ":", lines[[1]], ":", lines[[2]])

  paste0(
    "\\testonly{\n",
    "testex::testex(style = \"testthat\", srcref = ", deparse(src), ", \n",
    deparse_pretty(test),
    ")}",
    trimws(substring(tag$raw, n + 1L), "right")
  )
}

#' Populate An Implicit `@testthat` Lambda Function
#'
#' When a `testthat` tag does not contain a `.` object, its result is
#' onsidered an an implicit `testthat` expectation, which should be injected
#' with a `.` as a first argument.
#'
#' @param expr A (possibly) implicity lambda function
#' @return A new expression, injecting a `.` argument if needed.
#'
#' @noRd
#' @keywords internal
populate_testthat_dot <- function(expr) {
  if (is.expression(expr)) expr <- expr[[1]]
  if (!"." %in% all.names(expr)) {
    expr <- as.call(append(as.list(expr), quote(.), after = 1L))
  }
  expr
}



#' Find The Last Character of the First Expression
#'
#' @param x A parsed expression with [`srcref`].
#' @return An integer representing the character position of the end of the
#'   first call in a in a parsed expression.
#'
#' @noRd
#' @keywords internal
first_expr_end <- function(x) {
  if (!is.null(sr <- attr(x[[1]], "wholeSrcref"))) {
    nchar(paste0(as.character(sr), collapse = "\n"))
  } else if (!is.null(sr <- attr(x, "wholeSrcref"))) {
    nchar(paste0(as.character(sr), collapse = "\n"))
  }
}



#' Escape R Documentation `\\testonly` Strings
#'
#' @param x A `character` value
#' @return An escaped string, where any `\` is converted to `\\`
#'
#' @noRd
#' @family roclet_process_helpers
#' @keywords internal
escape_infotex <- function(x) {
  gsub("\\\\", "\\\\\\\\", x)
}
