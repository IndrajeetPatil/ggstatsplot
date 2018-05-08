#'
#' @title Default theme used in all `ggstatsplot` package plots
#' @name theme_mprl
#' @aliases theme_ggstatsplot
#' @author Indrajeet Patil
#' @description Common theme used across all plots generated in `ggstatsplot`
#'   and *assumed* by the author to be aesthetically pleasing to the
#'   user/reader.
#'
#' @return A `ggplot2` object with the `theme_mprl` theme.
#'
#' @import ggplot2
#' @import grid
#'
#' @export
#'

theme_mprl <- function() {
  ggplot2::theme_grey() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
      strip.text.x = ggplot2::element_text(size = 14, face = "bold"),
      strip.text.y = ggplot2::element_text(size = 14, face = "bold"),
      strip.text = ggplot2::element_text(size = 14, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 14, face = "bold"),
      axis.text.x = ggplot2::element_text(size = 14, face = "bold"),
      axis.text.y = ggplot2::element_text(size = 14, face = "bold"),
      axis.line = ggplot2::element_line(),
      legend.text = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 14, face = "bold"),
      legend.title.align = 0.5,
      legend.text.align = 0.5,
      legend.key.height = grid::unit(x = 1, units = "line"),
      legend.key.width = grid::unit(x = 1, units = "line"),
      plot.margin = grid::unit(x = c(1, 1, 1, 1), units = "lines"),
      panel.border = ggplot2::element_rect(
        color = "black",
        fill = NA,
        size = 1
      ),
      plot.title = ggplot2::element_text(
        color = "black",
        size = 16,
        face = "bold",
        hjust = 0.5
      ),
      plot.subtitle = ggplot2::element_text(
        color = "black",
        size = 12,
        face = "bold",
        hjust = 0.5
      )
    )
}
