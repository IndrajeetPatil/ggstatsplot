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
  data <- as_tibble(data) %>% tidyr::drop_na({{ grouping.var }})
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
  list(
    data = group_split(data),
    title = as.character(pull(group_keys(data), 1L))
  )
}


#' @noRd
.validate_palette <- function(palette, default = "ggthemes::gdoc") {
  if (!grepl("::", palette, fixed = TRUE)) {
    # nocov start
    rlang::warn(c(
      "!" = paste0(
        "Palette '",
        palette,
        "' is not in the required 'package::palette' format."
      ),
      i = paste0(
        "Ignoring it and using the default palette '",
        default,
        "' instead."
      ),
      "*" = "Update your code: combine package and palette into one string, e.g., `palette = \"ggsci::nrc_npg\"`."
    ))
    return(default)
  } # nocov end
  palette
}

#' @title Check if palette has enough number of colors
#'
#' @description
#' Aborts with an informative error if the number of factor levels exceeds the
#'   number of colors available in the specified palette.
#'
#' @examples
#' ggstatsplot:::.is_palette_sufficient("ggthemes::gdoc", 6L)
#' try(ggstatsplot:::.is_palette_sufficient("ggthemes::gdoc", 30L))
#'
#' @autoglobal
#' @keywords internal
.is_palette_sufficient <- function(palette, min_length) {
  parts <- strsplit(palette, "::", fixed = TRUE)[[1L]]
  d <- paletteer::palettes_d_names
  palette_length <- d[
    d$package == parts[[1L]] & d$palette == parts[[2L]],
    "length",
    drop = TRUE
  ]
  n_available <- if (length(palette_length) == 0L) 0L else palette_length

  if (n_available < min_length) {
    rlang::abort(c(
      x = paste0(
        "Palette '",
        palette,
        "' has only ",
        n_available,
        " colors, but ",
        min_length,
        " are needed."
      ),
      i = "Select a `palette` with enough colors. Run `View(paletteer::palettes_d_names)` to see options."
    ))
  }

  invisible(TRUE)
}


#' @noRd
.eval_f <- function(.f, ...) {
  tryCatch(
    suppressWarnings(suppressMessages(exec(.f, ...))),
    error = function(e) NULL
  )
}


#' @noRd
.extract_expression <- function(data) {
  purrr::pluck(data, "expression", 1L, .default = NULL)
}
