% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{deparse_indent}
\alias{deparse_indent}
\title{Deparse an expression and indent for pretty-printing}
\usage{
deparse_indent(x, indent = 0L)
}
\arguments{
\item{x}{A \code{code} object}

\item{indent}{An \code{integer} number of spaces or a string to prefix each
line of the deparsed output.}
}
\value{
An indented version of the deparsed string from \code{x}.
}
\description{
Deparse an expression and indent for pretty-printing
}
\keyword{internal}
