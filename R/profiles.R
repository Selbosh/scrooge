#' Calculate citation profiles
#'
#' For a directed network, calculate the outgoing transition probabilities of a random walker at each node.
#' This is equivalent to the proportion of citations each journal allocates to each other journal.
#'
#' @param x a matrix of citations (from columns to rows) or an [igraph][igraph::igraph] object
#' @param self logical. Include self-citations? If `FALSE`, they will not be counted.
#'
#' @return
#' A matrix of journal citation profiles.
#'
#' @seealso
#' [community_profile()]
#'
#' @examples
#' cprofile(citations)
#' cprofile(citations, self = FALSE)
#'
#' @export
cprofile <- function(x, self) UseMethod('cprofile', x)

#' @rdname cprofile
#' @importFrom Matrix diag Diagonal colSums
#' @import methods
#' @export
cprofile.default <- function(x, self = TRUE) {
  x <- as(x, 'dgCMatrix')
  if (!is.null(dimnames(x))) names(dimnames(x)) <- c('cited', 'citing')
  if (!self) diag(x) <- 0
  if (any(colSums(x) == 0)) warning('Empty columns ==> division by zero!')
  x %*% Diagonal(x = 1 / colSums(x))
}

#' @rdname cprofile
#' @importFrom igraph as_adjacency_matrix is.weighted V
#' @importFrom Matrix t diag
#' @export
cprofile.igraph <- function(x, self = TRUE) {
  x <- igraph::as_adjacency_matrix(graph = x,
                                   attr = switch(igraph::is.weighted(x), 'weight'),
                                   sparse = TRUE)
  x <- t(x)
  if (!self) diag(x) <- 0
  NextMethod('cprofile', x)
}

#' Calculate community profiles
#'
#' Given a directed network and a mapping of nodes to communities, calculate the *community profiles*
#' --- the vector of probabilities that a node from one community cites other nodes in the network.
#'
#' Profiles are computed by aggregating the out-links of nodes in each community, then scaling to sum
#' to one. This means nodes with more out-links have greater influence on their community profiles.
#'
#' @param network A matrix of citation counts (from columns to rows) or an [igraph][igraph::igraph] object.
#' @param \dots Further arguments passed to [merge_communities()].
#' @param self logical. Include self-citations? If `FALSE`, they will not be counted.
#'
#' @seealso
#' [merge_communities()], [cprofile()]
#'
#' @return
#' A sparse matrix of community profiles.
#' Each column corresponds to a community and each row to a node in the network.
#'
#' @export
community_profile <- function(network, ..., self = TRUE) {
  merged <- merge_communities(network, ...)
  cprofile(merged, self = self)
}
