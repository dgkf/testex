% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/use.R
\name{use_testex_as_testthat}
\alias{use_testex_as_testthat}
\title{Run examples as \code{testthat} expectations}
\usage{
use_testex_as_testthat(path = getwd(), context = "testex", quiet = FALSE)
}
\arguments{
\item{path}{A package source code working directory}

\item{context}{A \code{testthat} test context to use as the basis for a new test
filename.}

\item{quiet}{Whether to emit output messages.}
}
\value{
The result of \code{\link[=writeLines]{writeLines()}} after writing a new \code{testthat} file.
}
\description{
Run examples as \code{testthat} expectations
}
\concept{use}
