#' Simulate a quasi-symmetric matrix
#'
#' @param n the dimension of the matrix
#' @param lambda the mean of the underlying Poisson random number generator
#' @param density the density (i.e. proportion of non-zeroes) of the symmetric component
#' @param dimnames names of the matrix dimensions, as a list
#'
#' @importFrom Matrix rsparsematrix drop0
#'
#' @return A sparse, quasi-symmetric matrix of dimension \code{n} by \code{n}.
#'
#' @export
#'
#' @examples
#' rquasisymmetric(5)
rquasisymmetric <- function(n,
                            lambda = 10,
                            density = 1.00,
                            dimnames = list(cited = paste0('P', 1:n),
                                            citing = paste0('P', 1:n))) {
  S <- Matrix::rsparsematrix(n, n,
                             density = density,
                             symmetric = TRUE,
                             dimnames = dimnames,
                             rand.x = function(N) rpois(N, lambda))
  a <- runif(n)
  a <- setNames(a / sum(a), dimnames$cited)
  A <- diag(a)
  X <- Matrix::drop0(A %*% S)
  attr(X, 'S') <- S
  attr(X, 'a') <- a
  dimnames(X) <- dimnames
  X
}
