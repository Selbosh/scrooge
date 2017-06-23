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
#' @importFrom Matrix diag Diagonal colSums
#' @importFrom methods as
#' @export
cprofile.default <- function(x, self = TRUE) {
  x <- as(x, 'dgCMatrix')
  if (!is.null(dimnames(x))) names(dimnames(x)) <- c('cited', 'citing')
  if (!self) diag(x) <- 0
  if (any(colSums(x) == 0)) warning('Empty columns ==> division by zero!')
  x %*% Diagonal(x = 1 / colSums(x))
}

#' @rdname cprofile
#' @importFrom igraph as_adjacency_matrix is.weighted
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
