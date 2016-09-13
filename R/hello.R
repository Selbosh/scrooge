#' @title A Hello World Function
#'
#' @description
#' Give me an argument and I will return you a greeting.
#'
#' @details
#' If \code{friend} is not specified, the function
#'   will simply say "Hello!" instead.
#'
#' @concept selby
#'
#' @param friend A character string
#'
#' @return A character greeting
#'
#' @examples
#' hello()
#' hello('David')
#' hello('everyone')
#'
#' @export

hello <- function(friend = NULL) {
  if(is.null(friend)) message <- "Hello!"
  else message <- paste0("Hello, ", friend, '!')
  print(message)
}
