#' Anticipate use of tibbles
#'
#' See \code{\link[tibble]{tibble}} for details.
#'
#' @name tibble
#' @keywords internal
#' @export
#' @importFrom tibble tibble
NULL

#' @title Transform object of any other class to an object of class `ggplot`.
#' @name ggplot_converter
#'
#' @param plot A plot that needs to be converted to object of class `ggplot`.
#'
#' @importFrom cowplot ggdraw draw_grob
#' @importFrom grid grobTree
#'
#' @examples
#' library(ggplot2)
#' 
#' # creating a plot that is not of class `ggplot`
#' p <- ggExtra::ggMarginal(ggplot(mtcars, aes(wt, mpg)) + geom_point())
#' 
#' # checking class of object
#' class(p)
#' 
#' # checking class of converted plot
#' p_converted <- ggstatsplot::ggplot_converter(p)
#' class(p_converted)
#' @export

# function body
ggplot_converter <- function(plot) {
  # convert the saved plot
  p <- cowplot::ggdraw() +
    cowplot::draw_grob(grid::grobTree(plot))

  # returning the converted plot
  return(p)
}
