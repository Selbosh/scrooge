% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quasi_symm.R
\name{rquasisymmetric}
\alias{rquasisymmetric}
\title{Simulate a quasi-symmetric matrix}
\usage{
rquasisymmetric(n, lambda = 10, density = 1, dimnames = list(cited =
  paste0("P", 1:n), citing = paste0("P", 1:n)))
}
\arguments{
\item{n}{the dimension of the matrix}

\item{lambda}{the mean of the underlying Poisson random number generator}

\item{density}{the density (i.e. proportion of non-zeroes) of the symmetric component}

\item{dimnames}{names of the matrix dimensions, as a list}
}
\value{
A sparse, quasi-symmetric matrix of dimension \code{n} by \code{n}.
}
\description{
Simulate a quasi-symmetric matrix
}
\examples{
rquasisymmetric(5)
}
