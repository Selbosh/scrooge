#' Compute a PageRank vector
#'
#' Given a (weighted) adjacency matrix, compute the PageRank: the
#'   the stationary distribution of a random walk around the graph.
#'
#' @param C a square matrix
#' @param alpha a damping factor
#' @param row2col by default, c_{ij} refers to the directed edge that
#'   points from column j to row i. Set \code{TRUE} to
#'   transpose so that edges lead from rows to columns instead
#' @param sort reorder the indices in descending order of PageRank
#'
#' @return A PageRank vector, scaled to sum to one
#'
#' @importFrom rARPACK eigs
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
