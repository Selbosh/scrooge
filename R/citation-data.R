#' Academic citations dataset
#'
#' A square matrix of citation counts between 47 academic journals in the field of statistics in 2010.
#'
#'  Citations point from the column journal (\code{citing}) to the row journal (\code{cited}).
#'  The \emph{article counts} (that is, the total number of publications from that journal in 2010) are available via \code{articles}.
#'
#' @docType data
#'
#' @usage data(citations)
#'
#' @keywords datasets
#'
#' @references
#' Varin, C., Cattelan, M., & Firth, D. (2016).
#' Statistical modelling of citation exchange between statistics journals.
#' \emph{Journal of the Royal Statistical Society: Series A (Statistics in Society)},
#' \bold{179(1)}, 1--63.
#'
#' @seealso \code{\link[BradleyTerry2]{citations}}, a similar, smaller dataset in \code{BradleyTerry2}
"citations"

#' @rdname citations
#' @usage data(articles)
"articles"
