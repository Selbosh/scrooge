#' Calculate the log-likelihood of a Bradley-Terry model
#'
#' @param mu an estimate of ability scores
#' @param X a matrix of paired comparisons (weights)
#' @family Bradley-Terry model utility functions
loglikelihood <- function(mu, X) {
  diag(X) <- 0
  sum(diag(mu) %*% X - X * log(exp(mu)[row(X)] + exp(mu)[col(X)]))
}

#' Calculate the observed Fisher information of Bradley-Terry model estimates
#'
#' @importFrom numDeriv hessian
#' @family Bradley-Terry model utility functions
#'
#' @param mu a vector of Bradley--Terry model parameter estimates
#' @param X a square matrix of paired comparisons
hessianBT <- function(mu, X) {
  numDeriv::hessian(function(t) -loglikelihood(t, X), mu)
}

#' Calculate the variance-covariance matrix for a Bradley-Terry model
#'
#' @return A Fisher information matrix
#' @family Bradley-Terry model utility functions
#'
#' @param mu a vector of Bradley--Terry model parameter estimates
#' @param X a square matrix of paired comparisons
#'
#' @examples
#' mu <- BTscores(citations)
#' vcovBT(mu, citations)
#'
#' @export
vcovBT <- function(mu, X) {
  solve(hessianBT(mu, X)[-1,-1])
}

#' Add pseudocounts to a paired comparison matrix
#'
#' Allows fitting \code{\link{BradleyTerry}} models with parameters equal to damped \code{\link{Scroogefactor}}s.
#'
#' Eigenvector estimators can be used to compute Bradley--Terry scores.
#' But a \emph{damped} PageRank uses a modified probability transition matrix dependent on a parameter, \code{alpha}.
#'
#' @seealso \code{\link{BradleyTerry}}
#' @family Bradley-Terry model utility functions
#'
#' @examples
#' SF <- Scroogefactor(citations, alpha = 0.85)
#' citations_pseudo <- pseudocount(citations)
#' BT_pseudo <- BTscores(citations_pseudo)
#' plot(SF, BT_pseudo); abline(0, 1)
#'
#' @param X a square matrix of paired comparison data
#' @param alpha a damping factor between 0 and 1
#'
#' @return a matrix in the same form as \code{X}, with pseudocounts added
#'
#' @export
pseudocount <- function(X, alpha = 0.85) {
  n <- nrow(X)
  H <- alpha * diag(n) + (1 - alpha)/n * matrix(1, n, n)
  H %*% X
}
