#' Add `testex` tags and configure package to fully use `testex` features
#'
#' @note
#' The testex roxygen tags behave similarly to 'roxygen2' `@examples` tags,
#' with the minor addition of some wrapping code to manage the tests. This
#' means that they will be integrated into your `@examples` and can be
#' intermixed between `@examples` tags
#'
#' @param path A package source code working directory
#' @param check A \code{logical} value indicating whether tests should be
#'   executing during \code{R CMD check}.
#' @param quiet Whether output should be suppressed
#'
#' @return The result of [`write.dcf()`] upon modifying the package
#'   `DESCRIPTION` file.
#'
#' @export
use_testex <- function(path = getwd(), check = TRUE, quiet = FALSE) {
  path <- file.path(find_package_root(path), "DESCRIPTION")
  desc <- read.dcf(path)
  desc <- read.dcf(path, keep.white = colnames(desc))

  report <- report(quiet)
  desc <- update_desc_roxygen(desc, report)
  desc <- update_desc_suggests(desc, report)
  desc <- update_desc_config_options(desc, list(check = check), report)
  update_testthat(path, report)
  report$show("Configuring {.pkg testex}:")

  write.dcf(
    desc,
    path,
    keep.white = setdiff(colnames(desc), "Roxygen"),
    width = 80L,
    indent = 2L
  )
}



#' A Simple Stateful Reporter Class
#'
#' @param quiet Whether output should be shown
#' @return A class-like environment with a few reporting methods
#'
#' @noRd
report <- function(quiet) {
  messages <- character(0L)
  add <- function(..., .envir = parent.frame()) {
    messages <<- append(messages, cliless(..., .envir = .envir))
  }
  show <- function(title) {
    if (quiet) return()
    title <- cliless(title)
    if (length(messages) > 0) {
      cat(title, "\n", paste0(" * ", messages, collapse = "\n"), "\n", sep = "")
    } else {
      cat(title, "You're already set up!\n")
    }
  }
  environment()
}



#' Update Roxygen field in DESCRIPTION
#'
#' @param desc A parsed DESCRIPTION matrix
#' @param report A reporter to aggregate output
#' @return Used for side-effects of updating DESCRIPTION and reporter
#'
#' @noRd
update_desc_roxygen <- function(desc, report) {
  # update Roxygen settings
  roxygen_orig <- if (!"Roxygen" %in% colnames(desc)) {
    list()
  } else {
    eval(
      parse(text = desc[1L, "Roxygen"], keep.source = FALSE),
      envir = new.env(parent = baseenv())
    )
  }

  # add testex to packages
  roxygen <- roxygen_orig
  roxygen$packages <- unique(c(roxygen$packages, packageName()))
  if (!identical(roxygen, roxygen_orig)) {
    report$add(
      "Including {.code package = \"{packageName()}\"} in ",
      "{.code Roxygen} {.file DESCRIPTION} field"
    )
  }

  roxygen_str <- paste0("\n    ", deparse(roxygen), collapse = "")
  desc_update(desc, Roxygen = roxygen_str)
}

#' Update Suggests field to DESCRIPTION
#'
#' @param desc A parsed DESCRIPTION matrix
#' @param report A reporter to aggregate output
#' @return Used for side-effects of updating DESCRIPTION and reporter
#'
#' @noRd
update_desc_suggests <- function(desc, report) {
  # add testex to Suggests
  suggests <- if (!"Suggests" %in% colnames(desc)) {
    ""
  } else {
    desc[1L, "Suggests"]
  }

  package_re <- paste0("\\b", packageName(), "\\b")
  if (!any(grepl(package_re, suggests))) {
    lines <- Filter(nchar, strsplit(suggests, "\n")[[1]])
    ws <- min(nchar(gsub("[^ ].*", "", lines)), 4)
    package <- paste0(strrep(" ", ws), packageName())
    suggests <- paste0("\n", paste(c(lines, package), collapse = ",\n"))
    report$add("Adding {.code Suggests} package {.pkg {packageName()}}")
  }

  desc_update(desc, Suggests = suggests)
}

#' Add Config/pkg/options field to DESCRIPTION
#'
#' @param desc A parsed DESCRIPTION matrix
#' @param options Options to use
#' @param report A reporter to aggregate output
#' @return Used for side-effects of updating DESCRIPTION and reporter
#'
#' @noRd
update_desc_config_options <- function(desc, options, report) {
  config <- paste("Config", packageName(), "options", sep = "/")
  options_orig <- if (!config %in% colnames(desc)) {
    list()
  } else {
    eval(
      parse(text = desc[1L, config], keep.source = FALSE),
      envir = new.env(parent = baseenv())
    )
  }

  options_new <- options_orig
  options_new[names(options)] <- options[names(options)]
  if (!identical(options_new, options_orig)) {
    desc <- cbind(desc, matrix(NA_character_, dimnames = list(c(), config)))
    desc[1L, config] <- paste0("\n    ", deparse(options_new))
    report$add(
      "Configuring {.file DESCRIPTION} {.file {config}} with ",
      "{.code {deparse(options_new)}}"
    )
  }

  desc
}

#' Add testthat test for running example tests
#'
#' @param path A directory path to use as basis for finding testing suite
#' @param report A reporter to aggregate output
#' @return Used for side-effects of adding files and updating reporter
#'
#' @noRd
update_testthat <- function(path, report) {
  tryCatch(
    {
      f <- use_testex_as_testthat(path = path, quiet = TRUE)
      if (!is.null(f)) report$add("Adding test file {.file {f}}")
    },
    error = function(e) NULL
  )
}



#' Update Fields in the DESCRIPTION file
#'
#' @param desc A Parsed `DESCRPITION` file matrix
#' @param ... Named fields to update
#' @return A `DESCRIPTION` matrix
#'
#' @noRd
desc_update <- function(desc, ...) {
  cols <- list(...)
  new_cols <- setdiff(names(cols), colnames(desc))
  desc <- cbind(
    desc,
    matrix(
      NA_character_,
      nrow = nrow(desc),
      ncol = length(new_cols),
      dimnames = list(c(), new_cols)
    )
  )

  for (col in names(cols)) {
    desc[, col] <- cols[[col]]
  }

  desc
}



#' {cli}less
#'
#' Pretty format text without cli? As if! Call cli if available, or use a
#' heavily simplified version of glue, used as fallback.
#'
#' @param ... Used to form input string.
#' @param .envir Environment in which to evaluate expressions.
#' @return A formatted string.
#'
#' @noRd
cliless <- function(..., .envir = parent.frame(), .less = FALSE) {
  if (!.less && !is.null(tryCatch(ns <- getNamespace("cli"), error = function(e) NULL))) {
    return(ns$format_inline(..., .envir = .envir))
  }

  re <- "{(?:\\.([^{} ]+) )?([^{}]+|[^{]*(?R)[^}]*)}"
  str <- paste0(..., collapse = "")
  m <- gregexec(re, str, perl = TRUE)[[1]]
  if (!is.matrix(m)) return(str)
  l <- attr(m, "match.length")

  if (ncol(m) == 1 && m[1, 1] == 1 && l[1, 1] == nchar(str)) {
    # when entire string is a glue-ish cli expression
    style <- substring(str, s <- m[2, 1], s + l[2, 1] - 1L)
    expr <- substring(str, s <- m[3, 1], s + l[3, 1] - 1L)
    return(switch(style,
      "code" = paste0("`", cliless(expr, .envir = .envir, .less = .less), "`"),
      "file" = paste0("'", cliless(expr, .envir = .envir, .less = .less), "'"),
      "pkg" = paste0("{", cliless(expr, .envir = .envir, .less = .less), "}"),
      {
        text <- cliless(expr, .envir = .envir, .less = .less)
        format(eval(parse(text = text), envir = .envir))
      }
    ))
  }

  for (col in rev(seq_len(ncol(m)))) {
    start <- m[1, col]
    end <- start + l[1, col] - 1L
    str <- paste0(
      substring(str, 1L, start - 1L),
      cliless(substring(str, start, end), .envir = .envir, .less = .less),
      substring(str, end + 1L)
    )
  }

  str
}



#' Run examples as testthat expectations
#'
#' @param path A package source code working directory
#' @param context A testthat test context to use as the basis for a new test
#'   filename.
#' @param quiet Whether to emit output messages.
#'
#' @return The result of [`writeLines()`] after writing a new `testthat` file.
#'
#' @family use
#'
#' @importFrom utils packageName
#' @export
use_testex_as_testthat <- function(
    path = getwd(), context = "testex", quiet = FALSE) {
  path <- find_package_root(path)
  testthat_path <- file.path(path, "tests", "testthat")
  test_file <- file.path(testthat_path, paste0("test-", context, ".R"))

  if (!dir.exists(testthat_path)) {
    if (!quiet) {
      stop(
        "It looks like you don't have any testthat tests yet. Start ",
        "by setting up your package to use testthat, then try again."
      )
    }
    return()
  }

  if (file.exists(test_file)) {
    if (!quiet) {
      stop(sprintf(
        "testthat test file '%s' already exists.",
        basename(test_file)
      ))
    }
    return()
  }

  test_contents <- c(
    paste0(packageName(), "::test_examples_as_testthat()")
  )

  writeLines(test_contents, test_file)
  test_file
}
