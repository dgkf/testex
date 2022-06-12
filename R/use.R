#' @export
use_rd_roclet <- function(path = getwd(), check = NA) {
  path <- file.path(find_package_root(path), "DESCRIPTION")
  desc <- read.dcf(path)
  desc <- read.dcf(path, keep.white = colnames(desc))

  # update Roxygen settings
  roxygen <- if (!"Roxygen" %in% colnames(desc)) {
    list(markdown = TRUE, roclets = c("namespace"))
  } else {
    eval(
      parse(text = desc[1L,"Roxygen"], keep.source = FALSE),
      envir = new.env(parent = baseenv())
    )
  }

  roxygen$roclets <- c(
    setdiff(roxygen$roclets, c("rd", "testex::rd")),
    "testex::rd"
  )

  # add testex to Suggests
  suggests <- if (!"Suggests" %in% colnames(desc)) {
    character(0L)
  } else {
    desc[1L,"Suggests"]
  }

  if (!grepl("\\btestex\\b", suggests)) {
    suggests <- paste(c(suggests, "testex"), collapse = "\n")
  }

  desc[1L, "Roxygen"] <- paste0("\n    ", deparse(roxygen), collapse = "")
  desc[1L, "Suggests"] <- suggests
  write.dcf(
    desc,
    path,
    keep.white = setdiff(colnames(desc), "Roxygen"),
    width = 80L,
    indent = 2L
  )
}



#' @export
use_testex_as_testthat <- function(path = getwd(), context = "testex") {
  path <- find_package_root(path)
  package <- read.dcf(file.path(path, "DESCRIPTION"), fields = "Package")[[1L]]
  testthat_path <- file.path(path, "tests", "testthat")
  test_file <- file.path(testthat_path, paste0("test-", context, ".R"))

  if (!dir.exists(testthat_path)) {
    stop(
      "It looks like you don't have any testthat tests yet. Start ",
      "by setting up your package to use testthat, then try again."
    )
  }

  if (file.exists(test_file)) {
    stop(sprintf("testthat test file '%s' already exists.", test_file))
  }

  test_contents <- c(
    paste0(packageName(), "::test_examples_as_testthat(\"", package, "\")")
  )

  writeLines(test_contents, test_file)
}
