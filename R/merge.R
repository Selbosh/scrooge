#' Merge communities in a network
#'
#' Aggregate nodes in given communities into 'super-nodes' that represent those communities,
#'   and combine any associated inter- or intra-community links.
#'
#' Any matrix-like `x` argument will be coerced to a \code{\link[Matrix:dgCMatrix-class]{dgCMatrix}}.
#'
#' @inheritParams cprofile
#' @param communities A numeric vector that specifies the mapping, or a [communities][igraph::communities] object.
#' Its elements correspond to the vertices, and for each element the id in the new graph is given.
#'
#' @return
#' A sparse matrix, representing a contingency table of edges from communities to nodes.
#'
#' While it would be nice to return an [igraph][igraph::igraph] object when given an `x` of that class,
#'   the process of aggregating the 'from' nodes while preserving 'to' nodes would mean coercing
#'   the directed graph to an undirected bipartite graph, then contracting vertices in one mode
#'   and not the other. This may be difficult to implement, so for the time-being we simply convert
#'   igraph objects to weighted adjacency matrices and then pass to `merge_communities.default`.
#'
#' @export
merge_communities <- function(x, communities) {
  UseMethod('merge_communities', x)
}

#' @rdname merge_communities
#' @importFrom Matrix summary sparseMatrix
#' @importFrom stats aggregate
#' @importFrom igraph membership
#' @import methods
#' @export
merge_communities.default <- function(x, communities) {
  if (inherits(communities, 'communities'))
    communities <- igraph::membership(communities)
  network <- as(x, 'dgCMatrix')
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
merge_communities.igraph <- function(x, communities) {
  network <- igraph::as_adjacency_matrix(x,
                                         attr = switch(igraph::is.weighted(x), 'weight'),
                                         sparse = TRUE)
  network <- Matrix::t(network)
  NextMethod('merge_communities', network)
}
