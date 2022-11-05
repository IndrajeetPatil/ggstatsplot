#' @title Default theme used in `{ggstatsplot}`
#'
#' @description
#'
#' Common theme used across all plots generated in `{ggstatsplot}` and *assumed*
#' by the author to be aesthetically pleasing to the user/reader. The theme is a
#' wrapper around `theme_bw()`.
#'
#' All `{ggstatsplot}` functions have a `ggtheme` parameter that let you choose
#' a different theme.
#'
#' @return A `ggplot` object with the `theme_ggstatsplot` theme overlaid.
#'
#' @examples
#' library(ggplot2)
#' library(ggstatsplot)
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_ggstatsplot()
#' @export
theme_ggstatsplot <- function() {
  theme_bw(base_size = 10) +
    theme(
      axis.title         = element_text(face = "bold"),
      axis.title.y.right = element_text(size = 8),
      legend.title       = element_text(face = "bold"),
      plot.title         = element_text(size = 12, face = "bold"),
      panel.border       = element_blank(),
      strip.text         = element_text(face = "bold")
    )
}
