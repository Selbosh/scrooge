#' Themes for ggplot2 and stuff
#' 
#' To make everything look a bit nicer, we opt for Arial Narrow, white backgrounds and no tick marks.
#' 
#' @param base_size base font size
#' @param base_family base font family
#' 
#' @importFrom extrafont choose_font
#' @import ggplot2
#' 
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(iris) +
#'   aes(Petal.Width, Petal.Length, colour = Species) +
#'   geom_point() +
#'   ggtitle("Example plot of Fisher's iris data",
#'           subtitle = "Coloured by species of iris") +
#'   theme_selby()
#' }
#' 
#' @export
theme_selby <- function(base_size = 11, base_family = 'Arial Narrow') {
  base_family <- extrafont::choose_font(c(base_family, 'Tahoma', 'Arial', 'sans'), quiet = FALSE)
  thm <- ggplot2::theme_bw(base_family = base_family, base_size = base_size)
  thm <- thm + theme(plot.title = element_text(face = 'bold', size = rel(1.5)))
  thm <- thm + theme(axis.ticks = element_blank())
  return(thm)
}
