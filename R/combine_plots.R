#' @title Combining and arranging multiple plots in a grid
#' @name combine_plots
#'
#' @description
#'
#' Wrapper around `patchwork::wrap_plots` that will return a combined grid of
#' plots with annotations. In case you want to create a grid of plots, it is
#' **highly recommended** that you use `{patchwork}` package directly and not
#' this wrapper around it which is mostly useful with `{ggstatsplot}` plots. It
#' is exported only for backward compatibility.
#'
#' @return Combined plot with annotation labels
#'
#' @param plotlist A list containing `ggplot` objects.
#' @param plotgrid.args A `list` of additional arguments passed to
#'   `patchwork::wrap_plots`, except for `guides` argument which is already
#'   separately specified here.
#' @param annotation.args A `list` of additional arguments passed to
#'   `patchwork::plot_annotation`.
#' @param ... Currently ignored.
#' @inheritParams patchwork::wrap_plots
#'
#' @examples
#' library(ggplot2)
#'
#' # preparing the first plot
#' p1 <- ggplot(
#'   data = subset(iris, iris$Species == "setosa"),
#'   aes(x = Sepal.Length, y = Sepal.Width)
#' ) +
#'   geom_point() +
#'   labs(title = "setosa")
#'
#' # preparing the second plot
#' p2 <- ggplot(
#'   data = subset(iris, iris$Species == "versicolor"),
#'   aes(x = Sepal.Length, y = Sepal.Width)
#' ) +
#'   geom_point() +
#'   labs(title = "versicolor")
#'
#' # combining the plot with a title and a caption
#' combine_plots(
#'   plotlist = list(p1, p2),
#'   plotgrid.args = list(nrow = 1),
#'   annotation.args = list(
#'     tag_levels = "a",
#'     title = "Dataset: Iris Flower dataset",
#'     subtitle = "Edgar Anderson collected this data",
#'     caption = "Note: Only two species of flower are displayed",
#'     theme = theme(
#'       plot.subtitle = element_text(size = 20),
#'       plot.title = element_text(size = 30)
#'     )
#'   )
#' )
#' @export
combine_plots <- function(plotlist,
                          plotgrid.args = list(),
                          annotation.args = list(),
                          guides = "collect",
                          ...) {
  exec(patchwork::wrap_plots, !!!plotlist, guides = guides, !!!plotgrid.args) +
    exec(patchwork::plot_annotation, !!!annotation.args)
}
