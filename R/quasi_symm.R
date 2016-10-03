#' Simulate a quasi-symmetric matrix
#'
#' @param n the dimension of the matrix
#' @param intensity the mean of the underlying Poisson random number generator
#'
#' @importFrom Matrix rsparsematrix
#'
#' @return A sparse, quasi-symmetric matrix of dimension \code{n} by \code{n}.
#'
#' @export
#'
#' @examples
#' rquasisymmetric(5)
rquasisymmetric <- function(n, intensity = 10) {
  S <- Matrix::rsparsematrix(n, n, density = 0.7,
                             symmetric = TRUE,
                             dimnames = list(cited = letters[1:n], citing = letters[1:n]),
                             rand.x = function(N) rpois(N, intensity))
  a <- runif(n)
  A <- diag(a)
  X <- Matrix::drop0(A %*% S)
  attr(X, 'S') <- S
  attr(X, 'a') <- a/sum(a)
  X
}
