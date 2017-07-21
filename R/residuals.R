#' Calculate Pearson profile residuals
#'
#' Residuals are calculated using the formula
#' \deqn{r_i = \frac{y_i - \hat{y}_i}{\sqrt{\hat{y}_i}}}{(y_i - E[y_i]) / \sqrt{E[y]_i}}
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
#' @family functions for residual analysis of communities
#' @seealso [stats::rstandard.glm()]
#'
#' @references
#' Agresti, Alan (2002).
#'   *Categorical Data Analysis* (2nd ed., pp. 366--367).
#'   New York, NY: Wiley.
#'
#' @examples
#' # Compare citations of 'Biometrika' to those of all stats journals
#' Bka_counts <- citations[, 'Bka']
#' stats_profile <- rowSums(citations) / sum(citations)
#' mean_counts <- mean_profile * sum(Bka_counts)
#' profile_residuals(mean_counts, Bka_counts)
#'
#' @export
profile_residuals <- function(expected, observed) {
  stopifnot(length(expected) == length(observed))
  if (any(expected < 0) || any(observed < 0))
    stop('Counts data must be non-negative')
  if (sum(expected) != sum(observed))
    warning('Total number of citations expected is not equal to the total observed')
  raw_residual <- observed - expected
  if (any(observed[expected == 0] > 0))
    warning('Citations observed when expected rate is zero')
  ifelse(expected != 0,
         raw_residual / sqrt(expected),
         0)
}

#' Predict citations based on community profile
#'
#' Based on the nearest community profile, what is the expected distribution of citations
#' from journal `idx`?
#'
#' To vectorise this function, it might be easier first to create a vectorised version of
#' [nearest_point()].
#'
#' @return A vector of predicted outgoing citations from journal `idx`.
#'
#' @inheritParams nearest_point
#' @inheritParams cprofile
#'
#' @importFrom igraph strength
#' @importFrom Matrix colSums
#'
#' @examples
#' distances <- as.dist(1 - cor(citations + t(citations) - diag(diag(citations))))
#' clusters <- cutree(hclust(distances), h = 0.6)
#' fitted_citations(NULL, citations, clusters)
#' fitted_citations('Bka', citations, clusters)
#'
#' @export
fitted_citations <- function(idx = NULL, citations, communities, self = TRUE) {
  idx <- get_journal_IDs(idx, citations)
  if (inherits(citations, 'igraph'))
    outcitations <- igraph::strength(citations)[idx]
  else
    outcitations <- Matrix::colSums(citations)[idx]
  nprofs <- nearest_profile(idx, citations, communities, self)
  nprofs * rep(outcitations, each = nrow(nprofs))
}

#' Calculate residuals for communities in a network
#'
#' @param x a matrix of citations (from columns to rows) or an [igraph][igraph::igraph] object
#' @inheritParams nearest_point
#'
#' @family functions for residual analysis of communities
community_residuals <- function(citations, communities) {
  NULL
}
