#' @title Split data frame into a list by grouping variable.
#'
#' @description
#'
#' This function splits the data frame into a list, with the length of the list
#' equal to the factor levels of the grouping variable.
#'
#' @inheritParams ggbetweenstats
#' @param grouping.var A single grouping variable.
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' ggstatsplot:::.grouped_list(ggplot2::msleep, grouping.var = vore)
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
#' @noRd
.palette_message <- function(package, palette, min_length) {
  palette_length <- paletteer::palettes_d_names %>%
    filter(package == !!package, palette == !!palette) %$%
    length[[1L]]

  are_enough_colors_available <- palette_length > min_length

  if (!are_enough_colors_available) {
    rlang::warn(c(
      "Number of labels is greater than default palette color count.",
      "Select another color `palette` (and/or `package`)."
    ))
  }

  return(are_enough_colors_available)
}

#' @noRd
.eval_f <- function(.f, ...) {
  tryCatch(
    suppressWarnings(suppressMessages(exec(.f, ...))),
    error = function(e) NULL
  )
}
