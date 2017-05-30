#' Fit a Bradley--Terry model
#'
#' Fits Bradley--Terry model, also known as a quasi-symmetry model,
#' to paired comparison data via maximum (quasi) likelihood estimation.
#'
#' This implementation avoids calling \code{\link[BradleyTerry2]{BTm}} and
#' instead fits the \code{\link[stats]{glm}} directly.
#' By default, the model is fitted by quasi-likelihood (allowing non-integer counts) with a logit link.
#' To fit by maximum likelihood instead, set the \code{\link[stats]{family}} argument to \code{binomial}.
#'
#' @param C a square matrix of paired comparisons
#' @param family a description of the error distribution and link function to be used. See \code{\link[stats]{family}}
#'
#' @return An object of class \code{"glm"}
#'
#' @family network centrality estimators
#' @family Bradley-Terry model utility functions
#'
#' @examples
#' BradleyTerry(citations)
#'
#' @references
#' Bradley, R. A., & Terry, M. E. (1952).
#' Rank analysis of incomplete block designs: I. The method of paired comparisons.
#' \emph{Biometrika},
#' 39(3/4), 324--345.
#'
#' @importFrom stats glm quasibinomial
#'
#' @export
BradleyTerry <- function(C, family = stats::quasibinomial) {
  C <- as.matrix(C) # Bit of a hack for now
  n <- nrow(C)
  Y <- as.matrix(cbind(win1 = t(C)[lower.tri(C)],
                       win2 = C[lower.tri(C)]))
  npairs <- nrow(Y)
  X <- matrix(0, npairs, n)
  colnames(X) <- colnames(C)
  # +1 for player1, -1 for player2, 0 otherwise
  X[cbind(1:npairs, col(C)[lower.tri(C)])] <- 1
  X[cbind(1:npairs, row(C)[lower.tri(C)])] <- -1
  X <- X[, -1] # First column redundant
  glm(Y ~ -1 + X, family = family)
}

#' Estimate the parameters of a Bradley--Terry model
#'
#' A quick way to retrieve Bradley--Terry parameter estimates without saving the \code{glm} object.
#'
#' For a model summary and standard errors, use \code{\link{BradleyTerry}}
#'
#' @param X a matrix of paired comparison data
#' @param sort logical.
#'
#' @family network centrality estimators
#'
#' @examples
#' BTscores(citations)
#'
#' @importFrom stats coef setNames
#'
#' @export
BTscores <- function(X, sort = FALSE) {
  fit <- BradleyTerry(X)
  scores <- setNames(c(0, coef(fit)), colnames(X))
  scores <- scores - mean(scores)
  expscores <- exp(scores) / sum(exp(scores))
  expscores
}

#' Estimate the parameters of a Bradley--Terry model
#'
#' A one-liner to fit and extract model parameters from a \code{\link[BradleyTerry2]{BTm}} model
#' Deprecated: use \code{\link{BradleyTerry}} or \code{\link{BTscores}} instead.
#'
#' Previously a placeholder function, now remains for reference and comparison purposes.
#'
#' @param C a square matrix of paired comparison data
#' @param sort logical. If \code{TRUE}, sort the weights in descending order
#'
#' @seealso BradleyTerry BTscores
#'
#' @examples
#' BT2(citations)
#'
#' # Compare:
#' plot(BT2(citations), BTscores(citations))
#' abline(0, 1)
#'
#' @importFrom BradleyTerry2 countsToBinomial BTm
#' @importFrom stats setNames
#'
#' @references
#' Heather Turner, David Firth (2012).
#' Bradley--Terry Models in R: The \code{BradleyTerry2} Package.
#' \emph{Journal of Statistical Software},
#' 48(9), 1--21.
#'
#' Bradley, R. A., & Terry, M. E. (1952).
#' Rank analysis of incomplete block designs: I. The method of paired comparisons.
#' \emph{Biometrika},
#' 39(3/4), 324--345.
#'
#' @export
BT2 <- function(C, sort = FALSE) {
  bin_df <- BradleyTerry2::countsToBinomial(as.matrix(C))
  BT_model <- BradleyTerry2::BTm(outcome = cbind(win1, win2), player1 = player1, player2 = player2, data = bin_df)
  mu <- setNames(c(0, coef(BT_model)), colnames(C))
  mu <- mu - mean(mu)
  expmu <- exp(mu) / sum(exp(mu))
  if(sort) sort(expmu, decreasing = TRUE) else expmu
}
