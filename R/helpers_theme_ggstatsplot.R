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
        axis.title.x = ggplot2::element_text(face = "bold"),
        axis.title.y = ggplot2::element_text(face = "bold")
      ) +
      common_theme_element
  } else {
    ggtheme
  }
}

# common theme element for all default themes
common_theme_element <-
  ggplot2::theme(
    legend.text = ggplot2::element_text(size = 10),
    legend.title = ggplot2::element_text(size = 10, face = "bold"),
    plot.title = ggplot2::element_text(size = 12, face = "bold"),
    plot.subtitle = ggplot2::element_text(size = 10)
  )

#' @rdname theme_ggstatsplot
#' @aliases theme_ggstatsplot
#' @export

theme_corrmat <- function() {
  ggplot2::theme() + common_theme_element
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
        strip.text = ggplot2::element_text(size = 10, face = "bold")
      ) +
      common_theme_element
  } else {
    ggtheme
  }
}
