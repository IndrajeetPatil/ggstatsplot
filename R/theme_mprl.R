#' @title default theme to use for `ggstatsplot` package figures
#' @name theme_mprl
#' @aliases theme_mprl
#' @author Indrajeet Patil
#' @description Common theme used across all plots generated in `ggstatsplot` and assumed by the author to be
#' aesthetically pleasing to the user/reader.
#'
#' @import ggplot2
#' @import grid
#'
#' @export

theme_mprl <- function() {
  theme_grey() +
    theme(
      axis.title.x = element_text(size = 14, face = "bold"),
      strip.text.x = element_text(size = 14, face = "bold"),
      strip.text.y = element_text(size = 14, face = "bold"),
      strip.text = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 14, face = "bold"),
      axis.line = element_line(),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 14, face = "bold"),
      legend.title.align = 0.5,
      legend.text.align = 0.5,
      legend.key.height = unit(1, "line"),
      legend.key.width = unit(1, "line"),
      plot.margin = unit(c(1, 1, 1, 1), "lines"),
      # requires library(grid))
      panel.border = element_rect(
        colour = "black",
        fill = NA,
        size = 1
      ),
      plot.title = element_text(
        color = "black",
        size = 16,
        face = "bold",
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        color = "black",
        size = 12,
        face = "bold",
        hjust = 0.5
      )
    )
}
