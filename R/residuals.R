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
