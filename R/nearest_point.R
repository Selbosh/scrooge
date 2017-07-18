#' Calculate nearest point in convex hull
#'
#' Given a citation matrix and a community structure, this function calculates the point
#' in the convex hull of community profiles that is nearest to a given journal profile.
#'
#' This function uses quadratic programming to calculate the closest point by Euclidean distance.
#' The `citations` matrix should be arranged so that citations are directed from columns to rows.
#' If `idx` is a name, it should correspond to a row name from `citations`.
#'
#' @param idx A journal name or index.
#' @param citations A square contingency table of citation data.
#' @param communities A membership vector or [igraph::communities] object.
#' @param self logical. Include self-citations? If `FALSE`, they will not be counted.
#'
#' @examples
#' counts <- citations[1:6, 1:6]
#' comms <- setNames(c(1, 2, 3, 2, 2, 4), colnames(counts))
#' nearest_point('AoS', counts, comms) # Inside hull (value == 0), because AoS is itself a community.
#' nearest_point('ANZS', counts, comms) # Outside hull, near to its own community.
#'
#' # To which cluster should 'Biometrika' belong?
#' distances <- as.dist(1 - cor(citations + t(citations)))
#' clusters <- cutree(hclust(distances), h = 0.8)
#' result <- nearest_point('Bka', citations, clusters)
#' # Verify Euclidean distance is calculated correctly:
#' point <- community_profile(citations, clusters) %*% result$solution
#' result$value %=% sum((citations[, 'Bka'] / sum(citations[, 'Bka']) - point)^2)
#'
#' @seealso
#' [quadprog::solve.QP()], [community_profile()]
#'
#' @importFrom quadprog solve.QP
#' @export
nearest_point <- function(idx, citations, communities, self = TRUE) {
  stopifnot(length(idx) == 1) # May vectorise later
  C <- community_profile(citations, communities, self = self)
  g <- ncol(C) # number of communities
  P <- cprofile(citations, self = self)
  D <- t(C) %*% C
  d <- P[, idx] %*% C
  A <- t(rbind(rep(1, g),
               diag(g)))
  b <- c(1, rep(0, g))

  soln <- quadprog::solve.QP(Dmat = D, dvec = d, Amat = A, bvec = b, meq = 1)
  soln$value <- base::drop(2 * soln$value + t(P[, idx]) %*% P[, idx]) # re-add constant term p^T p
  soln$cosine <- 1 - soln$value / 2
  soln
}

#' Calculate nearest point by cosine similarity
#'
#' Applying EM to each individual point. Like a separate mixture model for every journal.
#'
#' Find the nearest [citation profile][cprofile()] to `x` that is a convex combination of
#' the community profiles `y`.
#'
#' @inheritParams nearest_point
#'
#' @details
#' In order for an \eqn{n}-vector to be a [citation profile][cprofile()],
#'   the elements must be non-negative and sum to one.
#' This is also true for any convex combination (finite mixture distribution) of citation profiles.
#'
#' Geometrically, this represents a point on part of the surface of the unit \eqn{n-1}-sphere
#'   that is within the positive [closed orthant][https://en.wikipedia.org/wiki/Orthant]
#'   in \eqn{\mathbb{R}^n}{R^n}.
#' In a 3-journal network, this corresponds to the eighth of the unit sphere in the first octant and
#'   in a 2-journal network, the quarter of the unit circle in the first quadrant.
#'
#' The largest possible angle between two valid citation profiles is
#'   \eqn{\theta = \frac{\pi}{2}}{\theta = \pi/2}, with cosine similarity
#'   \eqn{\cos\theta = 0}{cos \theta = 0}.
#'   For example, the most well-separated profiles in 2-d are \eqn{(1, 0)} and \eqn{(0, 1)} ---
#'   i.e. the \eqn{x}-axis and the \eqn{y}-axis.
#'
#' @return
#' An object exactly like that returned by [nearest_point()].
#' Use `$cosine` to extract the cosine similarity.
#'
#' @seealso [nearest_point()], [cosine_similarity()]
#'
#' @export
nearest_cosine <- nearest_point


#' Multinomial mixture probabilities
#'
#' This is equivalent to performing the initial "E" step in expectation maximisation (EM)
#' for a multinomial mixture model.
#'
#' Here we are effectively computing the nearest point in the convex hull of community profiles,
#' implicitly using Kullback--Leibler divergence as the distance measure.
#' Alternative distance measures may be used; see [nearest_point()] and others.
#'
#' @seealso [nearest_point()]
#'
#' @param target A vector of citations to be compared
#' @param profiles A matrix of [community profiles()]
#'
#' @return
#' A vector of log-probabilities that each community generated `target`'s citation profile.
#'
#' @examples
#' # To which cluster should 'Biometrika' belong?
#' distances <- as.dist(1 - cor(citations + t(citations)))
#' clusters <- cutree(hclust(distances), h = 0.8)
#' profiles <- community_profile(citations, clusters)
#' Biometrika <- citations[, 'Bka']
#' w <- multinomial_mix(Biometrika, profiles)
#' which.max(w) == clusters['Bka']
#' profiles %*% exp(w) # nearest point
#'
#' @export
multinomial_mix <- function(target, profiles) {
  # Calculate log density of each multinomial model
  logf <- apply(profiles * sum(target), 2, function(j) dmultinom(target, prob = j, log = TRUE))
  # Safely normalise the densities using the 'log-sum-exp' trick
  a <- max(logf)
  logsumf <- a + log(sum(exp(logf - a)))
  logf - logsumf # = log(f / sum(f))
}

#' Cosine similarity between two vectors
#'
#' This function computes the *cosine similarity* between two vectors, defined by
#' \deqn{\cos(\theta) = \frac{x \cdot y}{\|x\|_2\|y\|_2}.}{cos(\theta) = (x . y) / (|x| |y|).}
#'
#' Cosine similarity is related to Euclidean distance by
#' \deqn{\|x - y \|^2 = 2(1 - \cos(\theta)).}{|x - y|^2 = 2(1 - cos(\theta)).}
#' So \deqn{\cos(\theta) = 1 - \frac12\|x - y\|^2,}{cos(\theta) = 1 - 0.5 * |x - y|^2,}
#' assuming `x` and `y` have been normalised to be unit vectors.
#' Therefore, if we want to *maximise* cosine similarity, we can minimise Euclidean distance
#' and then make the conversion. See [nearest_point()].
#'
#' @param x A numeric vector
#' @param y A numeric vector, the same length as `x`
#'
#' @return
#' Generally, a scalar between \eqn{-1} and \eqn{1}.
#' Or, if `x` and `y` are non-negative, a value between \eqn{0} and \eqn{1}.
#'
#' @references
#' [https://en.wikipedia.org/wiki/Cosine_similarity]
#'
#' @seealso [nearest_cosine()], [cos()], [acos()]
#'
#' @export
cosine_similarity <- function(x, y) {
  x %*% y / sqrt(x %*% x * y %*% y)
}

#' Calculate Pearson profile residuals
#'
#' Residuals are calculated using the formula
#' \deqn{r_i = \frac{y_i - \hat{y}_i}{\sqrt \hat{y}_i}}{(y_i - E[y_i]) / \sqrt{E[y]_i}}
#' which treats each element of the citation counts vector as a Poisson variate.
#'
#' We might want to make a generic S3 function like [stats::residuals()].
#' *Deviance* residuals may be added later. For now we have Pearson residuals only.
#'
#' @param expected Predicted vector of citations, based on some model
#' @param observed Vector of observed citations from the journal of interest
#'
#' @return A numeric vector of standardised residuals, the same length as `expected` and `observed`
#'
#' @seealso [stats::rstandard.glm()]
#'
#' @references
#' Agresti, Alan (2002).
#'   *Categorical Data Analysis* (2nd ed., pp. 366--367).
#'   New York, NY: Wiley.
#'
#' @examples
#' Bka_counts <- citations[, 'Bka']
#' mean_profile <- rowSums(citations) / sum(citations)
#' mean_counts <- mean_profile * sum(Bka_counts)
#' profile_residuals(mean_counts, Bka_counts)
#'
#' @export
profile_residuals <- function(expected, observed) {
  stopifnot(length(expected) == length(observed))
  stopifnot(is.numeric(expected) && is.numeric(observed))
  if (any(expected < 0) || any(observed < 0))
    stop('Counts data must be non-negative')
  if (sum(expected) != sum(observed))
    warning('Total number of citations expected is not equal to the total observed')
  raw_residual <- observed - expected
  raw_residual / sqrt(expected)
}

#' Index of dissimilarity
#'
#' @param x A numeric vector
#' @param y A numeric vector, the same length as `x`
#'
#' @return A number between \eqn{0} and \eqn{1}. Lower is better.
#'
#' @references
#' Kuha, Jouni and Firth, David (2011).
#' On index of dissimilarity for lack of fit in loglinear and log-multiplicative models.
#' *Computational Statistics \& Data Analysis*, 55(1):375--388.
#'
#' @seealso [profile_residuals()]
#'
#' @export
dissimilarity <- function(x, y) {
  stopifnot(sum(x) == sum(y))
  stopifnot(length(x) == length(y))
  abs(x - y) / 2 / sum(x)
}
