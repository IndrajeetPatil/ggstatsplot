#' @title Split data frame into a list by grouping variable
#'
#' @description
#'
#' This function splits the data frame into a list, with the length of the list
#' equal to the factor levels of the grouping variable.
#'
#' @inheritParams ggbetweenstats
#' @param grouping.var A single grouping variable.
#'
#' @autoglobal
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' ggstatsplot:::.grouped_list(ggplot2::msleep, grouping.var = vore)
#' @keywords internal
.grouped_list <- function(data, grouping.var) {
  data <- as_tibble(data)
  grp_col <- pull(data, {{ grouping.var }})
  grp_fct <- if (is.factor(grp_col)) {
    grp_col
  } else if (is.character(grp_col)) {
    forcats::fct_inorder(grp_col)
  } else {
    factor(grp_col)
  }
  data <- mutate(data, {{ grouping.var }} := grp_fct) %>%
    group_by({{ grouping.var }})
  list(data = group_split(data), title = as.character(pull(group_keys(data), 1L)))
}


#' @title Check if palette has enough number of colors
#'
#' @description
#' Informs the user about not using the default color palette
#'   when the number of factor levels is greater than 8, the maximum number of
#'   colors allowed by `"Dark2"` palette from the `{RColorBrewer}` package.
#'
#' @examples
#' ggstatsplot:::.is_palette_sufficient("RColorBrewer", "Dark2", 6L)
#' ggstatsplot:::.is_palette_sufficient("RColorBrewer", "Dark2", 12L)
#'
#' @autoglobal
#' @keywords internal
.is_palette_sufficient <- function(package, palette, min_length) {
  palette_length <- paletteer::palettes_d_names %>%
    filter(package == !!package, palette == !!palette) %>%
    purrr::pluck("length")

  are_enough_colors_available <- palette_length > min_length

  if (!are_enough_colors_available) {
    rlang::warn(c(
      x = "Number of labels is greater than default palette color count.",
      i = "Select another color `palette` (and/or `package`)."
    ))
  }

  are_enough_colors_available
}


#' @noRd
.eval_f <- function(.f, ...) {
  tryCatch(
    suppressWarnings(suppressMessages(exec(.f, ...))),
    error = function(e) NULL
  )
}


#' @noRd
.extract_expression <- function(data) purrr::pluck(data, "expression", 1L, .default = NULL)
