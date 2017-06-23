
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
#' @param communities an integer vector giving the community of each journal, or an \code{igraph} \code{\link[igraph]{communities}} object
#'
#' @return
#' A matrix of community profiles.
#' @seealso
#' \code{\link{cprofile}}, \code{\link{nearest_point}}
#' @examples
#' counts <- citations[1:6, 1:6]
#' comms <- setNames(c(1, 2, 3, 2, 2, 4), colnames(counts))
#' community_profiles(counts, comms)
#'
#' @export
community_profiles <- function(x, communities, self) {
  UseMethod('community_profiles', communities)
}

#' @rdname community_profiles
#' @importFrom stats aggregate xtabs
#' @importFrom Matrix summary Diagonal
#' @export
community_profiles.default <- function(x, communities, self = TRUE) {
  cite_prof <- cprofile(x, self)
  stopifnot(length(communities) == ncol(cite_prof))
  comm_prof <- Matrix::summary(cite_prof)
  comm_prof$j <- communities[comm_prof$j]
  comm_sizes <- table(communities)
  result <- stats::xtabs(x ~ ., data = comm_prof, sparse = TRUE) %*% Diagonal(x = 1 / comm_sizes)
  dimnames(result) <- list(cited = rownames(cite_prof), community = seq_along(comm_sizes))
  result
}

#' @rdname community_profiles
#' @importFrom igraph groups
#' @export
community_profiles.communities <- function(x, communities, self = TRUE) {
  communities <- igraph::membership(communities)
  NextMethod('community_profiles', communities)
}
