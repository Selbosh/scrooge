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
#' By default, \code{C[i,j]} refers to the directed edge that points from column j to row i.
#' Use \code{t(C)} if you want edges directed from rows to columns instead.
#'
#' @param C a square matrix
#' @param alpha a damping factor
#' @param sort logical. Reorder the indices in descending order of PageRank
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
                     sort = FALSE) {
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

#' Compute the Scroogefactor
#'
#' The Scroogefactor is PageRank divided by out-degree. It can be used as an approximate estimator for the Bradley--Terry model.
#'
#' Pinksi and Narin (1976) first proposed this metric as a citation metric called the \emph{influence per reference}.
#' When applied to citation data, it can be interpreted as the influence per outgoing reference, effectively penalising larger
#' publications and review journals, which have larger combined bibliographies. When applied to sports or games results, it is
#' influence per game lost.
#'
#' @param C a square matrix
#' @param alpha a damping factor
#' @param sort logical. Reorder the indices in descending order of Scroogefactor score
#'
#' @return A vector equivalent to PageRank per reference, scaled to sum to one
#'
#' @references
#' Pinski, G., & Narin, F. (1976).
#' Citation influence for journal aggregates of scientific publications: Theory, with application to the literature of physics.
#' \emph{Information Processing & Management},
#' 12(5), 297--312.
#'
#' @examples
#' Scroogefactor(citations, alpha = 1, sort = TRUE)
#'
#' @export
Scroogefactor <- function(C, alpha = 0.85, sort = FALSE) {
  SF <- PageRank(C, alpha = alpha, sort = FALSE) / colSums(C)
  SF <- SF / sum(SF)
  if(sort) sort(SF, decreasing = TRUE) else SF
}
