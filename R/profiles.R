#' Calculate citation profiles
#'
#' For a directed network, calculate the outgoing transition probabilities of a random walker at each node.
#' This is equivalent to the proportion of citations each journal allocates to each other journal.
#'
#' @param x a matrix of citations (from columns to rows) or an \code{igraph} object
#' @param self logical. Include self-citations? If \code{FALSE}, they will not be counted.
#'
#' @return
#' A matrix of journal citation profiles.
#'
#' @seealso
#' \code{\link{community_profiles}}
#'
#' @examples
#' cprofile(citations)
#' cprofile(citations, self = FALSE)
#'
#' @export
cprofile <- function(x, self) UseMethod('cprofile', x)

#' @rdname cprofile
#' @export
cprofile.default <- function(x, self = TRUE) {
  if (!self) diag(x) <- 0
  sweep(x, 2, colSums(x), '/')
}

#' @rdname cprofile
#' @importFrom igraph as_adjacency_matrix is.weighted
#' @export
cprofile.igraph <- function(x, self = TRUE) {
  x <- igraph::as_adjacency_matrix(x, attr = switch(igraph::is.weighted(x), 'weight'))
  x <- t(as.matrix(x))
  if (!self) diag(x) <- 0
  NextMethod('cprofile', x)
}

#' Calculate community profiles
#'
#' Given a citation data set, calculate the \emph{community profiles} --- the vector of probabilities
#' that a community cites each of the journals in the network.
#'
#' Currently this function calculates a community's profile from the (unweighted) mean of its
#' consistuent journal profiles. More sophisticated approaches might be more appropriate, because this means
#' that very large journals with many outgoing citations carry equal weight to small journals with very
#' few citations. Currently journals cannot belong to more than one community at a time.
#'
#' @inheritParams cprofile
#' @param communities an integer vector giving the community of each journal, or a \code{list} of members in each community. See examples.
#'
#' @return
#' A matrix of community profiles.
#' @seealso
#' \code{\link{cprofile}}, \code{\link{nearest_point}}
#' @examples
#' test <- citations[1:6, 1:6]
#' community_profiles(test, c(3, 3, 1, 3, 2, 3), self = FALSE)
#' community_profiles(test,
#'                    list(`1` = 'AoS', `2` = 'Bern', `3` = c('AmS', 'AISM', 'ANZS', 'BioJ')),
#'                    self = FALSE)
#' @export
community_profiles <- function(x, communities, self) {
  UseMethod('community_profiles', communities)
}

#' @rdname community_profiles
#' @importFrom stats aggregate xtabs
#' @export
community_profiles.default <- function(x, communities, self = TRUE) {
  profs <- cprofile(x, self)
  profs <- as.data.frame.table(profs)
  levels(profs[, 2]) <- communities
  profs[, 2] <- factor(profs[, 2], levels = sort(levels(profs[, 2])))
  profs <- stats::aggregate(profs[[3]], by = profs[, 1:2], mean)
  stats::xtabs(x ~ ., data = profs)
}

#' @rdname community_profiles
#' @importFrom igraph groups
#' @export
community_profiles.communities <- function(x, communities, self = TRUE) {
  communities <- igraph::groups(communities)
  NextMethod('community_profiles', communities)
}
