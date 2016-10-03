#' Transform values on a linear scale
#'
#' @param x a numeric vector
#' @param codomain the minimum and maximum of the output scale
#'
#' @return a vector in the same format as \code{x}, centred and scaled to fit the \code{codomain}
#'
#' @family plot constructors
#'
#' @export
linearScale <- function(x, codomain = c(0, 1)) {
  domain <- range(x)
  codomain[1] + (x - domain[1]) * diff(codomain) / diff(domain)
}

#' Transform values on a logarithmic scale
#'
#' @param x a numeric vector
#' @param ... arguments to pass to \code{\link{linearScale}}
#'
#' @return a vector in the same format as \code{x}, centred and scaled logarithmically
#'
#' @family plot constructors
#'
#' @export
logScale <- function(x, ...) {
  linearScale(log(x), ...)
}

#' ASCII plots
#'
#' Create a \emph{Grammar of Graphics}-style visualisation without a graphical device!
#'   Simply \code{print} your results to the console as beautiful data-driven ASCII art. The
#'   renderer will automatically scale to the width of your R console. Currently there are
#'   no borders, titles, ticks, labels or legends; these may or may not be added in future.
#'
#' The rendering height is fixed to 25 characters by default, but can be tweaked
#'  via \code{ap$height <- newheight} where \code{ap} is your saved \code{asciiplot} object.
#'
#' @param df a data frame, ideally two continuous variables and a factor
#' @param aes a list of aesthetics \code{x, y} and (optionally) \code{shape}
#' @param geom initial geometry; only \code{"point"} so far implemented
#'
#' @return an \code{asciiplot} object, which can be \code{print}ed
#'
#' @family plot constructors
#'
#' @references
#' Wilkinson, L. (2006).
#' \emph{The Grammar of Graphics.}
#' Springer Science & Business Media.
#'
#' Wickham, H. (2010).
#' A layered grammar of graphics.
#' \emph{Journal of Computational and Graphical Statistics}
#' 19(1), 3--28.
#'
#' @concept ascii textplot utf8plot unicodeplot
#'
#' @examples
#' asciiplot(iris, aes=list(x='Sepal.Width', y='Sepal.Length', shape='Species'), geom='point')
#'
#' if(!requireNamespace('BradleyTerry2')) install.packages('BradleyTerry2')
#' asciiplot(ggplot2::diamonds, aes=list(x='carat', y='price', shape='cut'), geom='point')
#'
#' @export
asciiplot <- function(df, aes = NULL, geom = NULL) {
  stopifnot(is.data.frame(df))
  ascii <- structure(list(data = df), class = "asciiplot")
  ascii$aesthetics <- aes
  ascii$geometry <- geom
  ascii
}

#' @rdname asciiplot
#' @param x an \code{asciiplot} object
#' @param ... not used
#' @export
print.asciiplot <- function(x, ...) {
  if(is.null(x$height)) x$height <- 25
  if(is.null(x$data)) stop("No data in plot")
  if(is.null(x$statistics)) x$statistics <- identity
  if(is.null(x$scales)) x$scales <- list('x' = linearScale, 'y' = linearScale)
  if(is.null(x$geometry)) warning("No geometry selected")
  if(is.null(x$coord)) x$coord <- list(' ', x$height, getOption('width') / 2)
  if(is.null(x$aesthetics)) stop("No aesthetics defined")
  "All tests complete"
  canvas <- do.call(matrix, x$coord)
  x_vals <- x$scales$x(x$data[, x$aesthetics$x]) * (ncol(canvas) - 1) + 1
  y_vals <- nrow(canvas) - x$scales$y(x$data[, x$aesthetics$y]) * (nrow(canvas) - 1)
  if("point" %in% x$geometry) {
    if(!is.null(x$aesthetics$shape)) {
      canvas[cbind(y_vals, x_vals)] <- c('O', 'X', '-', '+', '.')[x$data[, x$aesthetics$shape]]
    } else canvas[cbind(y_vals, x_vals)] <- 'O'
  }
  render <- function(grid, hspace=' ') output <- apply(grid, 1, function(row) cat(c(row,'\n'), sep=hspace))
  render(canvas)
}
