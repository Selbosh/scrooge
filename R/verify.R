#' @title Verify if two numbers are approximately equal
#'
#' @description
#' Rounds a pair of objects to 5 decimal places
#'   and tests for equality, elementwise.
#'
#' @details
#' For an infix operator, use \code{a \%=\% b}.
#'   For more conventional evaluation, try \code{verify(a, b)}.
#'
#' @examples
#' pi %=% 3.141593
#' 0.333333 %=% (1/3)
#' all(1:5 + 1e-6 %=% 1:5)
#' verify(0.7777777, 7/9)
#'
#' @aliases approx equalish
#' @seealso \code{\link{all.equal}} \code{\link{Comparison}} \code{\link{identical}}
#'
#' @export
verify <- function(a, b) {
  round(a, 5) == round(b, 5)
}

#' @rdname verify
#' @export
isapprox <- verify

#' @rdname verify
#' @export
`%=%` <- verify
