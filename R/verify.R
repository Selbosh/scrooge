#' @title Compare two vectors numerically
#'
#' @description
#' Check if all elements of one vector are equal, larger or smaller than those of another vector,
#' allowing for errors in floating-point arithmetic.
#'
#' @details
#' \code{\%==\%} checks for (approximate) equality,
#' \code{\%>=\%} tests if `a` is greater than or equal to `b` and
#' \code{\%<=\%} tests the reverse.
#' A \code{\%<\%} or \code{\%>\%} operator would be redundant
#' (and conflict with `magrittr`).
#'
#' Be aware that some binary operators, such as \code{`/`} take precedence,
#' so make sure to wrap \code{a} and \code{b} in brackets where appropriate.
#' Use \code{verify(a, b)} for a conventional prefix operator.
#'
#' @param a the first vector to be compared
#' @param b the second vector to be compared
#'
#' @return `TRUE` or `FALSE` or `NA`
#'
#' @examples
#' 0.333333 %==% (1/3)
#' 0.999999 %<=% 1
#' -1e-16 %>=% 0
#' verify(pi, 3.141592654)
#'
#' @seealso \code{\link{all.equal}} \code{\link{Comparison}} \code{\link{identical}}
#'
#' @rdname verify
#' @export
verify <- function(a, b) all(abs(a - b) < .Machine$double.eps^.5)

#' @rdname verify
#' @export
`%==%` <- verify

#' @rdname verify
#' @export
`%>=%` <- function(a, b) all(a > b - .Machine$double.eps^.5)

#' @rdname verify
#' @export
`%<=%` <- function(a, b) all(a < b + .Machine$double.eps^.5)
