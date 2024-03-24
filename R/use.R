#' Replace default rd roclet with testex rd roclet
#'
#' @note
#' The testex roclet aims to be functionally identical to the default roxygen rd
#' roclet for any default roxygen tags. It replaces the default to intersperse
#' tests in the midst of existing \code{\\examples{...}} sections of Rd files.
#'
#' @param path A package source code working directory
#' @param check A \code{logical} value indicating whether tests should be
#'   executing during \code{R CMD check}.
#'
#' @return The result of [`write.dcf()`] upon modifying the package
#'   `DESCRIPTION` file.
#'
#' @export
use_testex <- function(path = getwd(), check = NA, quiet = FALSE) {
  msg <- if (quiet) identity else message
  na <- NA_character_
  path <- file.path(find_package_root(path), "DESCRIPTION")
  desc <- read.dcf(path)
  desc <- read.dcf(path, keep.white = colnames(desc))

  report <- report(quiet)
  desc <- update_desc_roxygen(desc, report)
  desc <- update_desc_suggests(desc, report)
  desc <- update_desc_config_options(desc, check, report)
  update_testthat(path, report)
  report$show("Configuring `testex`:")

  write.dcf(
    desc,
    path,
    keep.white = setdiff(colnames(desc), "Roxygen"),
    width = 80L,
    indent = 2L
  )
}

report <- function(quiet) {
  messages <- character()
  add <- function(message) {
    messages <<- append(messages, message)
  }
  show <- function(title) {
    out <- paste0(title, "\n", paste0(paste0(" * ", messages), collapse = "\n"))
    if (!quiet && length(messages)) {
      message(out)
    } else if (!length(messages)) {
      message(paste0("No changes made"))
    }
  }
  environment()
}


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
    msg <- sprintf(
      'Including `packages = "%s"` in Roxygen DESCRIPTION field',
      packageName()
    )
    report$add(msg)
  }

  roxygen_str <- paste0("\n    ", deparse(roxygen), collapse = "")
  desc_update(desc, Roxygen = roxygen_str)
}

update_desc_suggests <- function(desc, report) {
  # add testex to Suggests
  suggests <- if (!"Suggests" %in% colnames(desc)) {
    character(0L)
  } else {
    desc[1L, "Suggests"]
  }

  package_re <- paste0("\\b", packageName(), "\\b")
  if (!any(grepl(package_re, suggests))) {
    lines <- Filter(nchar, strsplit(suggests, "\n")[[1]])
    ws <- min(nchar(gsub("[^ ].*", "", lines)))
    package <- paste0(strrep(" ", ws), packageName())
    suggests <- paste(c(suggests, package), collapse = ",\n")
    report$add(sprintf('Adding Suggests package "%s"', packageName()))
  }

  desc_update(desc, Suggests = suggests)
}

update_desc_config_options <- function(desc, check, report) {
  config <- paste("Config", packageName(), "options", sep = "/")

  if (!config %in% colnames(desc)) {
    desc <- cbind(desc, matrix(NA_character_, dimnames = list(c(), config)))
    desc[1L, config] <- paste0("\n    ", deparse(list(check = TRUE)))
    msg <- sprintf(
      "Configuring DESCRIPTION %s to run testex on R CMD check by default",
      config
    )
    report$add(msg)
  } else if (is.logical(check) && !is.na(check)) {
    desc[1L, config] <- paste0("\n    ", deparse(list(check = check)))
    msg <- sprintf("Configuring DESCRIPTION %s", config)
    report$add(smg)
  }

  desc
}

update_testthat <- function(path, report) {
  tryCatch(
    {
      f <- use_testex_as_testthat(path = path, quiet = TRUE)
      report$add(sprintf("Adding test file '%s'", f))
    },
    error = function(e) NULL
  )
}

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



#' Run examples as testthat expectations
#'
#' @param path A package source code working directory
#' @param context A testthat test context to use as the basis for a new test
#'   filename.
#'
#' @return The result of [`writeLines()`] after writing a new `testthat` file.
#'
#' @family use
#'
#' @importFrom utils packageName
#' @export
use_testex_as_testthat <- function(path = getwd(), context = "testex", quiet = FALSE) {
  path <- find_package_root(path)
  package <- read.dcf(file.path(path, "DESCRIPTION"), fields = "Package")[[1L]]
  testthat_path <- file.path(path, "tests", "testthat")
  test_file <- file.path(testthat_path, paste0("test-", context, ".R"))

  if (!dir.exists(testthat_path)) {
    if (!quiet) stop(
      "It looks like you don't have any testthat tests yet. Start ",
      "by setting up your package to use testthat, then try again."
    )
    return()
  }

  if (file.exists(test_file)) {
    if (!quiet) stop(sprintf(
      "testthat test file '%s' already exists.",
      basename(test_file)
    ))
    return()
  }

  test_contents <- c(
    paste0(packageName(), "::test_examples_as_testthat()")
  )

  writeLines(test_contents, test_file)
  test_file
}
