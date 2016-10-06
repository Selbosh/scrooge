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
#' @family network centrality estimators
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
