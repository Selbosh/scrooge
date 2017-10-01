#' Calculate Pearson profile residuals
#'
#' Residuals are calculated using the formula
#' \deqn{r_i = \frac{y_i - \hat{y}_i}{\sqrt{\hat{y}_i}}}{(y_i - E[y_i]) / \sqrt{E[y]_i}}
#' which treats each element of the citation counts vector as a Poisson variate.
#' If the expected value is zero, `NA` is returned.
#' (You can decide for yourself whether to omit these observations, treat them as zero or use some other approach.)
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
#' average_profile <- rowSums(citations) / sum(citations)
#' expected_counts <- average_profile * sum(Bka_counts)
#' profile_residuals(expected_counts, Bka_counts)
#'
#' @export
profile_residuals <- function(expected, observed) {
  stopifnot(length(expected) == length(observed))
  if (any(expected < 0) || any(observed < 0))
    stop('Counts data must be non-negative')
  if (!isTRUE(all.equal(sum(observed), sum(expected), check.attributes = FALSE)))
    warning('Total number of citations expected is not equal to the total observed')
  raw_residual <- observed - expected
  if (any(observed[expected == 0] > 0))
    warning('Citations observed when expected rate is zero')
  ifelse(expected != 0,
         raw_residual / sqrt(expected),
         NA)
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
#' @family functions for residual analysis of communities
#' @export
fitted_citations <- function(idx = NULL, citations, communities, self = TRUE) {
  idx <- get_journal_IDs(idx, citations)
  if (inherits(citations, 'igraph'))
    outcitations <- igraph::strength(citations)[idx]
  else
    outcitations <- Matrix::colSums(citations)[idx]
  nprofs <- nearest_profile(idx, citations, communities, self)
  nprofs[nprofs < 0] <- 0
  nprofs * rep(outcitations, each = nrow(nprofs))
}

#' Calculate residuals for communities in a network
#'
#' Compute the residual sum of squares for each journal in a network, given a community structure.
#' @inheritParams cprofile
#' @inheritParams nearest_point
#' @family functions for residual analysis of communities
#' @examples
#' distances <- as.dist(1 - cor(citations + t(citations) - diag(diag(citations))))
#' clusters <- cutree(hclust(distances), h = 0.6)
#' cr <- community_residuals(citations, clusters)
#' plot(colSums(citations), cr, xlab = 'Journal size', ylab = 'Journal RSS', type = 'n')
#' text(colSums(citations), cr, labels = colnames(citations))
#' @export
community_residuals <- function(citations, communities, self = TRUE) {
  pr <- profile_residuals(expected = fitted_citations(NULL, citations, communities, self),
                          observed = citations)
  colSums(pr^2, na.rm = TRUE)
}
