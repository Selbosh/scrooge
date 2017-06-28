#' Merge communities in a network
#'
#' Aggregate nodes in given communities into 'super-nodes' that represent those communities,
#' and combine any associated inter- or intra-community links.
#'
#' Any matrix-like \code{network} object will be coerced to a sparse \code{\link[Matrix:dgCMatrix-class]{dgCMatrix}}.
#'
#' @param network A matrix of citation counts (from columns to rows) or an \code{\link[igraph]{igraph}} object.
#' @param communities A numeric vector that specifies the mapping, or a \code{\link[igraph]{communities}} object.
#' Its elements correspond to the vertices, and for each element the id in the new graph is given.
#'
#' @return
#' A sparse matrix, representing a contingency table of edges from communities to nodes.
#'
#' While it would be nice to return an \code{igraph} object when given an \code{igraph} network,
#' the process of aggregating the 'from' nodes while preserving 'to' nodes would mean coercing
#' the directed graph to an undirected bipartite graph, then contracting vertices in one mode
#' and not the other. This may be difficult to implement, so for the time-being we simply convert
#' \code{igraph} objects to weighted adjacency matrices and then pass to \code{merge_communities.default}.
#'
#' @export
merge_communities <- function(network, communities) {
  UseMethod('merge_communities', network)
}

#' @rdname merge_communities
#' @importFrom Matrix summary sparseMatrix
#' @importFrom stats aggregate
#' @importFrom igraph membership
#' @export
merge_communities.default <- function(network, communities) {
  if (inherits(communities, 'communities'))
    communities <- igraph::membership(communities)
  network <- as(network, 'dgCMatrix')
  long_df <- Matrix::summary(network)
  long_df$j <- communities[long_df$j]
  long_df <- stats::aggregate(x ~ ., long_df, sum)
  sparseMatrix(i = long_df$i, j = long_df$j, x = long_df$x,
               dimnames = list(cited = rownames(network), community = NULL))
}

#' @rdname merge_communities
#' @importFrom igraph as_adjacency_matrix is.weighted
#' @importFrom Matrix t
#' @export
merge_communities.igraph <- function(network, communities) {
  network <- igraph::as_adjacency_matrix(network,
                                         attr = switch(igraph::is.weighted(network), 'weight'),
                                         sparse = TRUE)
  network <- Matrix::t(network)
  NextMethod('merge_communities', network)
}
