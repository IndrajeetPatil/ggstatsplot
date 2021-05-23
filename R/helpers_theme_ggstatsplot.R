#' @title Default theme used in all `ggstatsplot` package plots
#'
#' @description
#'
#' Common theme used across all plots generated in `ggstatsplot` and *assumed*
#' by the author to be aesthetically pleasing to the user/reader. The theme is a
#' wrapper around `ggplot2::theme_bw()`.
#'
#' @return A `ggplot2` object with the `theme_ggstatsplot` theme overlaid.
#'
#' @import ggplot2
#'
#' @export

# function body
theme_ggstatsplot <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(face = "bold"),
      legend.text = ggplot2::element_text(size = 10),
      legend.title = ggplot2::element_text(size = 10, face = "bold"),
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10),
      panel.border = ggplot2::element_blank()
    )
}
