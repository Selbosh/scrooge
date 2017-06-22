#' Calculate nearest point in convex hull
#'
#' Given a citation matrix and a community structure, this function calculates the point
#' in the convex hull of community profiles that is nearest to a given journal profile.
#'
#' This function uses quadratic programming to calculate the closest point by Euclidean distance.
#' The \code{citations} matrix should be arranged so that citations are directed from columns to rows.
#' If \code{idx} is a name, it should correspond to a row name from \code{citations}.
#'
#' @param idx A journal name or index.
#' @param citations A square contingency table of citation data.
#' @param communities A membership vector or igraph \code{communities} object.
#' @param self logical. Include self-citations? If \code{FALSE}, they will not be counted.
#'
#' @examples
#' cites <- citations[1:6, 1:6]
#' comms <- list(`1` = 'AoS', `2` = 'Bern', `3` = c('AmS', 'AISM', 'ANZS', 'BioJ'))
#' nearest_point('AoS', cites, comms) # Inside hull (value == 0), because AoS is itself a community.
#' nearest_point('ANZS', cites, comms) # Outside hull, near to its own community.
#'
#' @seealso
#' \code{\link[quadprog]{solve.QP}}, \code{\link{community_profiles}}
#'
#' @importFrom quadprog solve.QP
#' @export
nearest_point <- function(idx, citations, communities, self = TRUE) {
  stopifnot(length(idx) == 1)
  P <- cprofile(citations, self = self)
  C <- community_profiles(citations, communities)
  g <- ncol(C) # number of communities

  D <- t(C) %*% C
  d <- P[, idx] %*% C
  A <- t(rbind(rep(1, g),
               diag(g)))
  b <- c(1, rep(0, g))

  soln <- quadprog::solve.QP(Dmat = D, dvec = d, Amat = A, bvec = b, meq = 1)
  soln$value <- soln$value + .5 * t(P[, idx]) %*% P[, idx] # add constant term p^T p / 2
  soln
}
