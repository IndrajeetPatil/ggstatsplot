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

#' @title Split data frame into a list by grouping variable.
#'
#' @description
#'
#' This function splits the data frame into a list, with the length of the list
#' equal to the factor levels of the grouping variable. Each element of the list
#' will be a tibble.
#'
#' @inheritParams ggbetweenstats
#' @param grouping.var A single grouping variable.
#'
#' @examples
#' \donttest{
#' ggstatsplot:::.grouped_list(ggplot2::msleep, grouping.var = vore)
#' }
#' @keywords internal
.grouped_list <- function(data, grouping.var = NULL) {
  data <- as_tibble(data)

  if (quo_is_null(enquo(grouping.var))) {
    return(data)
  }

  data %>% split(f = new_formula(NULL, enquo(grouping.var)), drop = TRUE)
}


#' @title Message if palette doesn't have enough number of colors.
#' @name .palette_message
#' @description Informs the user about not using the default color palette
#'   when the number of factor levels is greater than 8, the maximum number of
#'   colors allowed by `"Dark2"` palette from the `RColorBrewer` package.
#'
#' @noRd
.palette_message <- function(package, palette, min_length) {
  # computing the palette length
  filter(paletteer::palettes_d_names, package == !!package, palette == !!palette) %$%
    length[[1]] -> pl

  # check if insufficient number of colors are available in a given palette
  pl_message <- ifelse(pl < min_length, FALSE, TRUE)

  # inform the user
  if (!pl_message) {
    rlang::warn(paste0(
      "Number of labels is greater than default palette color count.",
      "Select another color `palette` (and/or `package`)."
    ))
  }

  invisible(pl_message)
}
