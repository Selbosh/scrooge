#' Compute a PageRank vector
#'
#' Given a (weighted) adjacency matrix, compute the PageRank: the
#'   the stationary distribution of a random walk around the graph.
#'
#' PageRank is an eigenvector centrality metric, equivalent to Pinski & Narin's \emph{influence weight} (1976)
#' with the addition of a damping factor \code{alpha}, which simulates a random surfer traversing the graph and
#' teleporting at any time with probability \code{1 - alpha}. The effect of the damping factor is to smooth out
#' any disconnected components or transient portions of the network.
#'
#' In bibliometrics, PageRank has also been implemented as the Eigenfactor Metric and as the SCImago Journal Rank.
#'
#' @param C a square matrix
#' @param alpha a damping factor
#' @param row2col logical. by default, \code{C[i,j]} refers to the directed edge that
#'   points from column j to row i. Set \code{TRUE} to
#'   transpose the matrix \code{C} so that edges lead from rows to columns instead
#' @param sort reorder the indices in descending order of PageRank
#'
#' @return A PageRank vector, scaled to sum to one
#'
#' @importFrom rARPACK eigs
#'
#' @examples
#' PageRank(citations)
#'
#' # Scroogefactor, an estimator for the Bradley-Terry model
#' PageRank(citations)/colSums(citations)
#'
#' @references
#' Pinski, G., & Narin, F. (1976).
#' Citation influence for journal aggregates of scientific publications: Theory, with application to the literature of physics.
#' \emph{Information Processing & Management},
#' 12(5), 297--312.
#'
#' Page, L., Brin, S., Motwani, R., & Winograd, T. (1999).
#' The PageRank citation ranking: bringing order to the web.
#' Technical Report, Stanford InfoLab.
#'
#' @export
PageRank <- function(C,
                     alpha = 0.85,
                     row2col = FALSE,
                     sort = FALSE) {
  if(row2col) C <- t(C)
  n <- sqrt(length(C))
  C <- matrix(C, n, n, dimnames = dimnames(C))
  P <- scale(C, center = FALSE, scale = colSums(C))
  G <- alpha * P + (1 - alpha) / n
  PR <- c(abs(rARPACK::eigs(G, 1)$vectors))
  names(PR) <- rownames(C)
  PR <- PR / sum(PR)
  if(sort) sort(PR, decreasing = TRUE)
  else PR
}
