% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nearest_point.R
\name{get_journal_IDs}
\alias{get_journal_IDs}
\title{Get all journal IDs if none specified}
\usage{
get_journal_IDs(idx = NULL, citations)
}
\arguments{
\item{idx}{A journal name or index. Vectorised for \code{nearest_profile()}}

\item{citations}{a matrix of citations (from columns to rows) or an \link[igraph:igraph]{igraph} object}
}
\value{
A vector of journal names
}
\description{
If \code{idx} is \code{NULL} then the function will attempt to select \emph{all} journals.
}
