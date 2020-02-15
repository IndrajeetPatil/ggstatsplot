#' @title Default theme used in all `ggstatsplot` package plots
#' @description Common theme used across all plots generated in `ggstatsplot`
#'   and *assumed* by the author to be aesthetically pleasing to the
#'   user/reader.
#'
#' @param ggtheme A function, `ggplot2` theme name. Default value is
#'   `ggplot2::theme_bw()`. Any of the `ggplot2` themes, or themes from
#'   extension packages are allowed (e.g., `ggthemes::theme_fivethirtyeight()`,
#'   `hrbrthemes::theme_ipsum_ps()`, etc.).
#' @param ggstatsplot.layer Logical that decides whether `theme_ggstatsplot`
#'   theme elements are to be displayed along with the selected `ggtheme`
#'   (Default: `TRUE`). `theme_ggstatsplot` is an opinionated theme layer that
#'   override some aspects of the selected `ggtheme`.
#'
#' @return A `ggplot2` object with the `theme_ggstatsplot` theme overlaid.
#'
#' @import ggplot2
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/theme_ggstatsplot.html}
#'
#' @export

# function body
theme_ggstatsplot <- function(ggtheme = ggplot2::theme_bw(), ggstatsplot.layer = TRUE) {

  # if ggstatsplot-specific layer is to be added on top of the default theme
  if (isTRUE(ggstatsplot.layer)) {
    ggtheme +
      ggplot2::theme(
        axis.title.x = ggplot2::element_text(size = 11, face = "bold"),
        axis.title.y = ggplot2::element_text(size = 11, face = "bold"),
        axis.text.x = ggplot2::element_text(size = 11, face = "bold"),
        axis.text.y = ggplot2::element_text(size = 11, face = "bold"),
        strip.text.x = ggplot2::element_text(size = 11, face = "bold"),
        strip.text.y = ggplot2::element_text(size = 11, face = "bold"),
        strip.text = ggplot2::element_text(size = 11, face = "bold"),
        axis.line = ggplot2::element_line(),
        legend.text = ggplot2::element_text(size = 11),
        legend.title = ggplot2::element_text(size = 11, face = "bold"),
        legend.title.align = 0.5,
        legend.text.align = 0.5,
        legend.key.height = ggplot2::unit(x = 1, units = "line"),
        legend.key.width = ggplot2::unit(x = 1, units = "line"),
        plot.margin = ggplot2::unit(x = c(1, 1, 1, 1), units = "lines"),
        panel.border = ggplot2::element_rect(
          color = "black",
          fill = NA,
          size = 1
        ),
        plot.title = ggplot2::element_text(
          color = "black",
          size = 13,
          face = "bold",
          hjust = 0.5
        ),
        plot.subtitle = ggplot2::element_text(
          color = "black",
          size = 10,
          face = "plain",
          hjust = 0.5
        )
      )
  } else {
    ggtheme
  }
}

#' @rdname theme_ggstatsplot
#' @aliases theme_ggstatsplot
#' @export

theme_pie <- function(ggtheme = ggplot2::theme_bw(), ggstatsplot.layer = TRUE) {
  if (isTRUE(ggstatsplot.layer)) {
    ggtheme +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_text(size = 10, face = "bold"),
        strip.text.y = ggplot2::element_text(size = 10, face = "bold"),
        strip.text = ggplot2::element_text(size = 10, face = "bold"),
        legend.text = ggplot2::element_text(size = 10),
        legend.title = ggplot2::element_text(size = 10, face = "bold"),
        legend.title.align = 0.5,
        legend.text.align = 0.5,
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.key = ggplot2::element_rect(size = 5),
        legend.key.size = ggplot2::unit(x = 1.5, units = "lines"),
        legend.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
        legend.box.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
        panel.border = ggplot2::element_rect(
          color = "black",
          fill = NA,
          size = 1
        ),
        plot.subtitle = ggplot2::element_text(
          color = "black",
          size = 10,
          face = "plain",
          hjust = 0.5
        ),
        plot.title = ggplot2::element_text(
          color = "black",
          size = 10,
          face = "bold",
          hjust = 0.5
        )
      )
  } else {
    ggtheme
  }
}


#' @rdname theme_ggstatsplot
#' @aliases theme_ggstatsplot
#' @export

theme_corrmat <- function() {
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    strip.text.x = ggplot2::element_text(size = 10, face = "bold"),
    strip.text.y = ggplot2::element_text(size = 10, face = "bold"),
    strip.text = ggplot2::element_text(size = 10, face = "bold"),
    axis.title.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = 10, face = "bold"),
    axis.text.y = ggplot2::element_text(size = 10, face = "bold"),
    axis.line = ggplot2::element_line(),
    legend.text = ggplot2::element_text(size = 12),
    legend.title = ggplot2::element_text(size = 12, face = "plain"),
    legend.title.align = 0.5,
    legend.text.align = 0.5,
    legend.key.height = ggplot2::unit(x = 1, units = "line"),
    legend.key.width = ggplot2::unit(x = 1, units = "line"),
    plot.margin = ggplot2::unit(x = c(1, 1, 1, 1), units = "lines"),
    panel.border = ggplot2::element_rect(
      color = "black",
      fill = NA,
      size = 1
    ),
    plot.title = ggplot2::element_text(
      color = "black",
      size = 10,
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = ggplot2::element_text(
      color = "black",
      size = 10,
      face = "plain",
      hjust = 0.5
    )
  )
}
