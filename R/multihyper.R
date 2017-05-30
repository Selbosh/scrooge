#' The Multinomial Hypergeometric Distribution
#'
#' Sample a fixed number of balls, without replacement, from multiple urns.
#'
#' Requesting a non-integer or negative number of balls, or more than are available in all the urns, will throw an error.
#' If \code{k} is a vector of length > 1, only the first element will be used.
#' Specifying only one or two urns will respectively return \code{k} or call \code{\link{rhyper}} and issue a warning.
#'
#' If \eqn{k = \sum n} then the function should return \code{n}.
#'
#' @param n a vector describing the number of balls in each urn
#' @param k the total number of balls to draw from the urns
#'
#' @return A vector or array corresponding to the number of balls sampled from each of the urns, in the same format as \code{n}.
#'
#' @seealso \code{\link{rhyper}} for sampling two different colours of balls from a single urn (equivalent to sampling one colour from two urns)
#'
#' @examples
#' rmultihyper(c(5, 10, 8), 6)
#'
#' # 90% sample of citation counts data
#' k <- round(0.9 * sum(citations))
#' rmultihyper(citations, k)
#'
#' @references
#' \url{http://stats.stackexchange.com/questions/199948/random-generation-from-multivariate-hypergeometric-distribution}
#'
#' @importFrom stats rhyper
#'
#' @export
rmultihyper <- function(n, k) {
  N <- sum(n)
  if (k > N) stop('Requested', k, 'balls but only', N, 'available in total')
  if (k < 0 | !is.numeric(k) | k != round(k)) stop('Invalid number of balls requested: ', k)
  m <- length(n)
  if (m == 2) {
    warning('Only two urns specified')
    return(rhyper(1, n[1], n[2], k))
  }
  if (m == 1) {
    warning('Only one urn specified')
    return(k)
  }
  n_other <- N - n[1]

  x <- rep(0, m)
  x[1] <- rhyper(1, n[1], n_other, k)

  for (urn in 2:(m-1)) {
    n_other <- n_other - n[urn]
    k <- k - x[urn - 1]
    x[urn] <- rhyper(1, n[urn], n_other, k)
  }
  x[m] <- k - x[m - 1]
  dim(x) <- dim(n)
  dimnames(x) <- dimnames(n)
  x
}
