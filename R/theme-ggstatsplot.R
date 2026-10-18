#' @title Default theme used in `{ggstatsplot}`
#'
#' @description
#'
#' Theme used by default in all `{ggstatsplot}` plots, built on
#' [`ggplot2::theme_bw()`]. Use the `ggtheme` argument of any `{ggstatsplot}`
#' function to choose a different theme.
#'
#' @returns A `{ggplot2}` theme object.
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_ggstatsplot()
#' @export
theme_ggstatsplot <- function() {
  theme_bw(base_size = 10) +
    theme(
      axis.title = element_text(face = "bold"),
      axis.title.y.right = element_text(size = 8),
      legend.title = element_text(face = "bold"),
      plot.title = element_text(size = 12, face = "bold"),
      panel.border = element_blank(),
      strip.text = element_text(face = "bold")
    )
}
