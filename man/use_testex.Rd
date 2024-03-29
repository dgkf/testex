% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/use.R
\name{use_testex}
\alias{use_testex}
\title{Add \code{\link{testex}} tags and configure package to fully use \code{\link{testex}} features}
\usage{
use_testex(path = getwd(), check = TRUE, quiet = FALSE)
}
\arguments{
\item{path}{A package source code working directory}

\item{check}{A \code{logical} value indicating whether tests should be
executing during \verb{R CMD check}.}

\item{quiet}{Whether output should be suppressed}
}
\value{
The result of \code{\link[=write.dcf]{write.dcf()}} upon modifying the package
\code{DESCRIPTION} file.
}
\description{
Add \code{\link{testex}} tags and configure package to fully use \code{\link{testex}} features
}
\note{
The \code{\link{testex}} \code{roxygen2} tags behave similarly to \code{roxygen2} \verb{@examples}
tags, with the minor addition of some wrapping code to manage the tests. This
means that they will be integrated into your \verb{@examples} and can be
intermixed between \verb{@examples} tags
}
