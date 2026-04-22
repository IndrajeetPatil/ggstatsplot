utils::globalVariables(".pre")


#' @title Shared data-preparation step for two-variable plot functions
#' @name .prep_data
#'
#' @description
#'
#' Selects the specified columns from the data frame, drops rows with missing
#' values, and optionally converts `x` to a factor with unused levels dropped.
#'
#' @inheritParams ggbetweenstats
#'
#' @return A data frame.
#'
#' @autoglobal
#' @noRd
.prep_data <- function(data, x, y, x_as_factor = FALSE) {
  data <- data |>
    select({{ x }}, {{ y }}) |>
    tidyr::drop_na()

  if (x_as_factor) {
    data <- mutate(data, {{ x }} := droplevels(as.factor({{ x }})))
  }

  data
}


# nocov start
#' @noRd
.make_grouped_fn <- function(.fn, .pre = NULL, guides = "collect") {
  function(
    data,
    ...,
    grouping.var,
    plotgrid.args = list(),
    annotation.args = list()
  ) {
    if (!is.null(.pre)) {
      data <- .pre(data, ...)
    }
    .grouped_list(data, {{ grouping.var }}) |>
      purrr::pmap(.f = .fn, ...) |>
      combine_plots(plotgrid.args, annotation.args, guides = guides)
  }
}
# nocov end

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
  data <- as_tibble(data) |> tidyr::drop_na({{ grouping.var }})
  grp_col <- pull(data, {{ grouping.var }})
  grp_fct <- if (is.factor(grp_col)) {
    grp_col
  } else if (is.character(grp_col)) {
    forcats::fct_inorder(grp_col)
  } else {
    factor(grp_col)
  }
  data <- mutate(data, {{ grouping.var }} := grp_fct) |>
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


#' @autoglobal
#' @noRd
.stabilize_x_factor <- function(data, ...) {
  dots_q <- rlang::enquos(...)
  x_q <- dots_q[["x"]] %||% dots_q[[1L]]
  if (!is.null(x_q) && rlang::is_symbol(rlang::quo_get_expr(x_q))) {
    x_name <- rlang::as_name(rlang::quo_get_expr(x_q))
    # nocov start
    x_lvls <- if (is.factor(data[[x_name]])) {
      levels(data[[x_name]])
    } else {
      sort(unique(data[[x_name]]))
    }
    # nocov end
    data[[x_name]] <- factor(data[[x_name]], x_lvls)
  }
  data
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


#' @noRd
.subtitle_caption <- function(
  .f,
  .f.args,
  type,
  bf.message,
  bf.condition = type == "parametric"
) {
  subtitle_df <- .eval_f(.f, !!!.f.args, type = type)
  subtitle <- .extract_expression(subtitle_df)
  caption_df <- NULL
  caption <- NULL

  if (bf.condition && bf.message) {
    caption_df <- .eval_f(.f, !!!.f.args, type = "bayes")
    caption <- .extract_expression(caption_df)
  }

  list(
    subtitle = subtitle,
    caption = caption,
    subtitle_df = subtitle_df,
    caption_df = caption_df
  )
}
