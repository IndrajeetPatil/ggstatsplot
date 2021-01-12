#' @title Simpler way to combine and arrange multiple plots in a grid
#' @name combine_plots2
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("stable")}
#'
#' Wrapper around `cowplot::plot_grid` that will return a plotgrid along with a
#' combination of title, caption, and annotation label. This is a simpler version
#' of the `combine_plots` function in this package.
#'
#' @return Combined plot with title and/or caption and/or annotation label
#'
#' @param ... Currently ignored.
#' @param plotlist A list of plots to display.
#' @inheritParams combine_plots
#' @param plotgrid.args A list of additional arguments to `cowplot::plot_grid`.
#' @param title.args,caption.args,sub.args A list of additional arguments
#'   provided to `title`, `caption` and `sub`, resp.
#'
#' @importFrom cowplot plot_grid add_sub ggdraw draw_label
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
#' combine_plots2(
#'   plotlist = list(p1, p2),
#'   plotlist.args = list(labels = c("(a)", "(b)")),
#'   title.text = "Dataset: Iris Flower dataset",
#'   caption.text = "Note: Only two species of flower are displayed",
#'   title.args = list(color = "red"),
#'   caption.args = list(color = "blue")
#' )
#' @export

# function body
combine_plots2 <- function(plotlist,
                           plotgrid.args = list(),
                           title.text = NULL,
                           title.args = list(size = 16, fontface = "bold"),
                           caption.text = NULL,
                           caption.args = list(size = 10),
                           sub.text = NULL,
                           sub.args = list(size = 12),
                           title.rel.heights = c(0.1, 1.2),
                           caption.rel.heights = c(1.2, 0.1),
                           title.caption.rel.heights = c(0.1, 1.2, 0.1),
                           ...) {

  # preparing the basic plot
  plot <-
    rlang::exec(
      .f = cowplot::plot_grid,
      plotlist = plotlist,
      !!!plotgrid.args
    )

  # preparing the title
  if (!is.null(title.text)) {
    title <-
      cowplot::ggdraw() +
      rlang::exec(
        .f = cowplot::draw_label,
        label = title.text,
        !!!title.args
      )
  }

  # preparing the caption
  if (!is.null(caption.text)) {
    caption <-
      cowplot::ggdraw() +
      rlang::exec(
        .f = cowplot::draw_label,
        label = caption.text,
        !!!caption.args
      )
  }

  # combining the basic plot with the either title or caption or title and
  # caption
  if (!is.null(title.text)) {
    if (!is.null(caption.text)) {
      # if both title and caption are needed
      plot <-
        cowplot::plot_grid(
          title,
          plot,
          caption,
          ncol = 1,
          rel_heights = title.caption.rel.heights
        )
    } else {
      # if only title is needed
      plot <-
        cowplot::plot_grid(
          title,
          plot,
          ncol = 1,
          rel_heights = title.rel.heights
        )
    }
  } else if (!is.null(caption.text)) {
    # if only caption is needed
    plot <-
      cowplot::plot_grid(
        plot,
        caption,
        ncol = 1,
        rel_heights = caption.rel.heights
      )
  }

  # finally adding sub if it's needed
  if (!is.null(sub.text)) {
    plot <-
      cowplot::ggdraw(
        rlang::exec(
          .f = cowplot::add_sub,
          plot = plot,
          label = sub.text,
          !!!sub.args
        )
      )
  }

  # return the final, combined plot
  plot
}
