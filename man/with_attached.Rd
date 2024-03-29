% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{with_attached}
\alias{with_attached}
\title{Temporarily attach a namespace}
\usage{
with_attached(ns, expr)
}
\arguments{
\item{ns}{A namespace or namespace name to attach}

\item{expr}{An expression to evaluate while the namespace is attached}
}
\value{
The result of evaluating \code{expr}
}
\description{
This function is primarily for managing attaching of namespaces needed for
testing internally. It is exported only because it is needed in code
generated within \code{Rd} files, but is otherwise unlikely to be needed.
}
