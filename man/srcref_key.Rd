% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_srcref.R
\name{srcref_key}
\alias{srcref_key}
\title{Convert a \code{\link{srcref}} to a \code{\link{character}} representation}
\usage{
srcref_key(x, nloc = 2, path = c("base", "root", "full"))
}
\arguments{
\item{x}{A \code{\link{srcref}} object}

\item{nloc}{The number of locations (\code{\link[utils:sourceutils]{utils::getSrcLocation}}) to use.
Defaults to 2, indicating starting and ending line number.}

\item{path}{A form of file path to use for the key. One of \code{"base"} for only
the basename of the source file path, \code{"root"} for a path relative to a
package root directory if found, or \code{"full"} for the full file path.}
}
\value{
A string hash of a \link{srcref}
}
\description{
Convert a \code{\link{srcref}} to a \code{\link{character}} representation
}
\keyword{internal}
