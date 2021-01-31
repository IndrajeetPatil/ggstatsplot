#' @title Combining and arranging multiple plots in a grid
#' @name combine_plots
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("maturing")}
#'
#' Wrapper around `patchwork::wrap_plots` that will return a combined grid of
#' plots with annotations.
#'
#' @return Combined plot with annotation labels
#'
#' @param plotlist A list containing `ggplot` objects.
#' @param plotgrid.args Additional arguments passed to `patchwork::wrap_plots`.
#' @param annotation.args Additional arguments passed to
#'   `patchwork::plot_annotation`.
#' @param ... Currently ignored.
#'
#' @importFrom patchwork wrap_plots plot_annotation
#' @importFrom rlang exec !!!
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/combine_plots.html}
#'
#' @examples
#' # loading the necessary libraries
#' library(ggplot2)
#'
#' # preparing the first plot
#' p1 <-
#'   ggplot2::ggplot(
#'     data = subset(iris, iris$Species == "setosa"),
#'     aes(x = Sepal.Length, y = Sepal.Width)
#'   ) +
#'   geom_point() +
#'   labs(title = "setosa")
#'
#' # preparing the second plot
#' p2 <-
#'   ggplot2::ggplot(
#'     data = subset(iris, iris$Species == "versicolor"),
#'     aes(x = Sepal.Length, y = Sepal.Width)
#'   ) +
#'   geom_point() +
#'   labs(title = "versicolor")
#'
#' # combining the plot with a title and a caption
#' combine_plots(
#'   plotlist = list(p1, p2),
#'   annotation.args = list(
#'     tag_levels = "a",
#'     title = "Dataset: Iris Flower dataset",
#'     subtitle = "Edgar Anderson collected this data",
#'     caption = "Note: Only two species of flower are displayed"
#'   )
#' )
#' @export

# function body
combine_plots <- function(plotlist,
                          plotgrid.args = list(),
                          annotation.args = list(),
                          ...) {
  rlang::exec(
    .fn = patchwork::wrap_plots,
    !!!plotlist,
    guides = "collect",
    !!!plotgrid.args
  ) +
    rlang::exec(.fn = patchwork::plot_annotation, !!!annotation.args)
}
