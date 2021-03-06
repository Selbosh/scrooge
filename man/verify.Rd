% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/verify.R
\name{verify}
\alias{verify}
\alias{\%==\%}
\alias{\%>=\%}
\alias{\%<=\%}
\title{Compare two vectors numerically}
\usage{
verify(a, b)

a \%==\% b

a \%>=\% b

a \%<=\% b
}
\arguments{
\item{a}{the first vector to be compared}

\item{b}{the second vector to be compared}
}
\value{
\code{TRUE} or \code{FALSE} or \code{NA}
}
\description{
Check if all elements of one vector are equal, larger or smaller than those of another vector,
allowing for errors in floating-point arithmetic.
}
\details{
\code{\%==\%} checks for (approximate) equality,
\code{\%>=\%} tests if \code{a} is greater than or equal to \code{b} and
\code{\%<=\%} tests the reverse.
A \code{\%<\%} or \code{\%>\%} operator would be redundant
(and conflict with \code{magrittr}).

Be aware that some binary operators, such as \code{`/`} take precedence,
so make sure to wrap \code{a} and \code{b} in brackets where appropriate.
Use \code{verify(a, b)} for a conventional prefix operator.
}
\examples{
0.333333 \%==\% (1/3)
0.999999 \%<=\% 1
-1e-16 \%>=\% 0
verify(pi, 3.141592654)

}
\seealso{
\code{\link{all.equal}} \code{\link{Comparison}} \code{\link{identical}}
}
