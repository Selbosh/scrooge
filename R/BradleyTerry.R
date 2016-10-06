#' Estimate the parameters of a Bradley-Terry model
#'
#' Estimate Bradley-Terry ability scores from paired comparison data using the I-LSR algorithm.
#' Avoids using \code{glm} and seems to be faster, but only approximates maximum likelihood.
#'
#' The function uses iterative Luce Spectral Ranking (I-LSR) to approximate the maximum likelihood ability weights of the Bradley-Terry model.
#' \emph{To do:} Add unit tests.
#'
#' @param C a square matrix of paired comparisons
#' @param maxits The maximum number of iterations in the I-LSR algorithm
#' @param tolerance The criterion for convergence of the algorithm
#' @param sort logical. If \code{TRUE}, sort the weights in descending order
#' @param verbose logical. If \code{TRUE}, show the number of iterations as a message
#'
#' @return A vector of estimated ability scores
#'
#' @examples
#' ILSR(citations)
#'
#' @references
#' Maystre, Lucas and Grossglauser, Matthias (2015).
#' Fast and accurate inference of Plackett--Luce models.
#' In \emph{Advances in Neural Information Processing Systems},
#' 172--180.
#'
#' @export
ILSR <- function(C, sort = FALSE, maxits = 100, tolerance = 1e-6, verbose = FALSE){
  n <- nrow(C)
  pi <- matrix(NA, nrow = maxits + 1, ncol = n)
  colnames(pi) <- colnames(C)
  pi[1, ] <- rep(1/n, n)

  # Iterative Luce Spectral Ranking
  for(k in 1:maxits) {
    Q <- C / (pi[k,][row(C)] + pi[k,][col(C)])
    P <- matrix(Q / colSums(Q), nrow(Q))
    pi[k+1,] <- abs(rARPACK::eigs(P, 1)$vectors)
    pi[k+1,] <- pi[k+1,] / sum(pi[k+1,])
    change <- sqrt(sum((pi[k+1,] - pi[k,])^2))
    if (change < tolerance) {
      if(verbose) message(paste('Converged in', k, 'iterations'))
      pi <- pi[1:(k+1),] # trim results
      break
    }
  }
  pi_out <- c(tail(pi, 1))
  names(pi_out) <- colnames(C)
  if (sort) {
    sort(pi_out, decreasing = TRUE)
  } else pi_out
}

#' Estimate the parameters of a Bradley-Terry model
#'
#' Estimate Bradley-Terry ability scores from paired comparison data using an iterative scaling algorithm.
#'
#' The current implementation, based on \code{BradleyTerry2}, is very slow and is basically a placeholder.
#'
#' @param C a square matrix of paired comparisons
#' @param sort logical. If \code{TRUE}, sort the weights in descending order
#'
#' @return The maximum likelihood ability scores estimate, scaled to sum to one
#'
#' @family Bradley-Terry model utility functions
#'
#' @examples
#' BradleyTerry(citations)
#'
#' @import BradleyTerry2
#'
#' @references
#' Bradley, R. A., & Terry, M. E. (1952).
#' Rank analysis of incomplete block designs: I. The method of paired comparisons.
#' \emph{Biometrika},
#' 39(3/4), 324--345.
#'
#' Heather Turner, David Firth (2012).
#' Bradley--Terry Models in R: The \code{BradleyTerry2} Package.
#' \emph{Journal of Statistical Software},
#' 48(9), 1--21.
#'
#' @export
BT2 <- function(C, sort = FALSE) {
  bin <- BradleyTerry2::countsToBinomial(C)
  BT_model <- BradleyTerry2::BTm(cbind(win1, win2), player1, player2, data = bin)
  mu <- setNames(c(0, coef(BT_model)), colnames(C))
  mu <- mu - mean(mu)
  expmu <- exp(mu) / sum(exp(mu))
  if(sort) sort(expmu, decreasing = TRUE) else expmu
}
#' @rdname BT2
#' @export
BT <- function(C) {
  n <- nrow(C)
  Y <- as.matrix(cbind(win1 = t(C)[lower.tri(C)],
                       win2 = C[lower.tri(C)]))
  npairs <- nrow(Y)
  # +1 for player1, -1 for player2, 0 otherwise
  X <- matrix(0, npairs, n)
  colnames(X) <- colnames(C)
  X[cbind(1:npairs, col(C)[lower.tri(C)])] <- 1
  X[cbind(1:npairs, row(C)[lower.tri(C)])] <- -1
  X <- X[, -1] # First column redundant
  glm(Y ~ -1 + X, family = quasibinomial)
}

#' @rdname BT2
#' @export
BTscores <- function(X) {
  fit <- BT(X)
  scores <- setNames(c(0, coef(fit)), colnames(X))
  scores <- scores - mean(scores)
  expscores <- exp(scores) / sum(exp(scores))
  expscores
}

#' Calculate the log-likelihood of a Bradley-Terry model
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
#' mu <- ILSR(citations)
#' vcovBT(mu, citations)
#'
#' @export
vcovBT <- function(mu, X) {
  solve(hessianBT(mu, X)[-1,-1])
}
