#' @title Combining and arranging multiple plots in a grid
#' @name combine_plots
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("stable")}
#'
#' Wrapper around \code{\link[cowplot]{plot_grid}} that will return a plotgrid
#' along with a combination of title, caption, and annotation label
#'
#' @return Combined plot with title and/or caption and/or annotation label
#'
#' @inheritDotParams cowplot::plot_grid
#' @param title.text String or plotmath expression to be drawn as title for the
#'   *combined plot*.
#' @param title.color Text color for title.
#' @param title.size Point size of title text.
#' @param title.vjust Vertical justification for title. Default = `0.5`
#'   (centered on `y`). `0` = baseline at `y`, `1` = ascender at `y`.
#' @param title.hjust Horizontal justification for title. Default = `0.5`
#'   (centered on `x`). `0` = flush-left at x, `1` = flush-right.
#' @param title.fontface The font face (`"plain"`, `"bold"` (default),
#'   `"italic"`, `"bold.italic"`) for title.
#' @param caption.text String or plotmath expression to be drawn as the caption
#'   for the *combined plot*.
#' @param caption.color Text color for caption.
#' @param caption.size Point size of title text.
#' @param caption.vjust Vertical justification for caption. Default = `0.5`
#'   (centered on y). `0` = baseline at y, `1` = ascender at y.
#' @param caption.hjust Horizontal justification for caption. Default = `0.5`
#'   (centered on x). `0` = flush-left at x, `1` = flush-right.
#' @param caption.fontface The font face (`"plain"` (default), `"bold"`,
#'   `"italic"`, `"bold.italic"`) for caption.
#' @param sub.text The label with which the *combined plot* should be annotated.
#'   Can be a plotmath expression.
#' @param sub.color Text color for annotation label (Default: `"black"`).
#' @param sub.size Point size of annotation text (Default: `12`).
#' @param sub.x The `x` position of annotation label (Default: `0.5`).
#' @param sub.y The `y` position of annotation label (Default: `0.5`).
#' @param sub.hjust Horizontal justification for annotation label (Default:
#'   `0.5`).
#' @param sub.vjust Vertical justification for annotation label (Default:
#'   `0.5`).
#' @param sub.vpadding Vertical padding. The total vertical space added to the
#'   label, given in grid units. By default, this is added equally above and
#'   below the label. However, by changing the y and vjust parameters, this can
#'   be changed (Default: `ggplot2::unit(1, "lines")`).
#' @param sub.fontface The font face (`"plain"` (default), `"bold"`, `"italic"`,
#'   `"bold.italic"`) for the annotation label.
#' @param sub.angle Angle at which annotation label is to be drawn (Default:
#'   `0`).
#' @param sub.lineheight Line height of annotation label.
#' @param title.caption.rel.heights Numerical vector of relative columns heights
#'   while combining (title, plot, caption).
#' @param title.rel.heights Numerical vector of relative columns heights while
#'   combining (title, plot).
#' @param caption.rel.heights Numerical vector of relative columns heights while
#'   combining (plot, caption).
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
#' combine_plots(
#'   p1, p2,
#'   labels = c("(a)", "(b)"),
#'   title.text = "Dataset: Iris Flower dataset",
#'   caption.text = "Note: Only two species of flower are displayed",
#'   title.color = "red",
#'   caption.color = "blue"
#' )
#' @export

# function body
combine_plots <- function(...,
                          title.text = NULL,
                          title.color = "black",
                          title.size = 16,
                          title.vjust = 0.5,
                          title.hjust = 0.5,
                          title.fontface = "bold",
                          caption.text = NULL,
                          caption.color = "black",
                          caption.size = 10,
                          caption.vjust = 0.5,
                          caption.hjust = 0.5,
                          caption.fontface = "plain",
                          sub.text = NULL,
                          sub.color = "black",
                          sub.size = 12,
                          sub.vjust = 0.5,
                          sub.hjust = 0.5,
                          sub.fontface = "plain",
                          sub.x = 0.5,
                          sub.y = 0.5,
                          sub.vpadding = ggplot2::unit(1, "lines"),
                          sub.angle = 0,
                          sub.lineheight = 0.9,
                          title.rel.heights = c(0.1, 1.2),
                          caption.rel.heights = c(1.2, 0.1),
                          title.caption.rel.heights = c(0.1, 1.2, 0.1)) {
  # preparing the basic plot
  plot <- cowplot::plot_grid(...)

  # preparing the title
  if (!is.null(title.text)) {
    title <-
      cowplot::ggdraw() +
      cowplot::draw_label(
        label = title.text,
        colour = title.color,
        size = title.size,
        vjust = title.vjust,
        hjust = title.hjust,
        fontface = title.fontface
      )
  }
  # preparing the caption
  if (!is.null(caption.text)) {
    caption <-
      cowplot::ggdraw() +
      cowplot::draw_label(
        label = caption.text,
        colour = caption.color,
        size = caption.size,
        vjust = caption.vjust,
        hjust = caption.hjust,
        fontface = caption.fontface
      )
  }

  # combining the basic plot with the either title or caption or title and
  # caption
  if (!is.null(title.text)) {
    if (!is.null(caption.text)) {
      # if both title and caption are needed
      plot <-
        cowplot::plot_grid(title,
          plot,
          caption,
          ncol = 1,
          rel_heights = title.caption.rel.heights
        )
    } else {
      # if only title is needed
      plot <-
        cowplot::plot_grid(title,
          plot,
          ncol = 1,
          rel_heights = title.rel.heights
        )
    }
  } else if (!is.null(caption.text)) {
    # if only caption is needed
    plot <-
      cowplot::plot_grid(plot,
        caption,
        ncol = 1,
        rel_heights = caption.rel.heights
      )
  }

  # finally adding sub if it's needed
  if (!is.null(sub.text)) {
    plot <-
      cowplot::ggdraw(
        cowplot::add_sub(
          plot = plot,
          label = sub.text,
          x = sub.x,
          y = sub.y,
          vpadding = sub.vpadding,
          colour = sub.color,
          size = sub.size,
          vjust = sub.vjust,
          hjust = sub.hjust,
          fontface = sub.fontface,
          angle = sub.angle,
          lineheight = sub.lineheight
        )
      )
  }

  # return the final, combined plot
  plot
}
