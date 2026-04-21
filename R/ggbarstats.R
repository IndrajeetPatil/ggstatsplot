#' @title Stacked bar charts with statistical tests
#' @name ggbarstats
#'
#' @description
#'
#' Bar charts for categorical data with statistical details included in the plot
#' as a subtitle.
#'
#' @section Summary of graphics:
#'
#' ```{r child="man/rmd-fragments/ggbarstats_graphics.Rmd"}
#' ```
#'
#' @inheritParams ggpiestats
#' @inheritParams ggbetweenstats
#' @param sample.size.label.args Additional aesthetic arguments that will be
#'   passed to [`ggplot2::geom_text()`].
#'
#' @inheritSection statsExpressions::contingency_table Contingency table analyses
#'
#' @inheritSection ggpiestats Pairwise comparisons
#'
#' @seealso \code{\link{grouped_ggbarstats}}, \code{\link{ggpiestats}},
#'  \code{\link{grouped_ggpiestats}}
#'
#' @inherit ggpiestats return details
#'
#' @autoglobal
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' # for reproducibility
#' set.seed(123)
#'
#' # one sample goodness of fit proportion test
#' p <- ggbarstats(mtcars, vs)
#'
#' # looking at the plot
#' p
#'
#' # extracting details from statistical tests
#' extract_stats(p)
#'
#' # association test (or contingency table analysis)
#' ggbarstats(mtcars, vs, cyl)
#'
#' # with 3+ x levels, pairwise comparisons are available
#' ggbarstats(mtcars, cyl, am)
#'
#' # Bayesian test
#' ggbarstats(mtcars, vs, cyl, type = "bayes")
#'
#' # using pre-aggregated data with counts
#' ggbarstats(as.data.frame(Titanic), x = Survived, y = Sex, counts = Freq)
#' @export
ggbarstats <- function(
  data,
  x,
  y = NULL,
  counts = NULL,
  type = "parametric",
  paired = FALSE,
  results.subtitle = TRUE,
  label = "percentage",
  label.args = list(alpha = 1.0, fill = "white"),
  sample.size.label.args = list(size = 4.0),
  digits = 2L,
  proportion.test = results.subtitle,
  digits.perc = 0L,
  bf.message = TRUE,
  ratio = NULL,
  alternative = "two.sided",
  conf.level = 0.95,
  p.adjust.method = "holm",
  sampling.plan = "indepMulti",
  fixed.margin = "rows",
  prior.concentration = 1.0,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  legend.title = NULL,
  xlab = NULL,
  ylab = NULL,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  palette = "ggthemes::gdoc",
  ggplot.component = NULL,
  ...
) {
  palette <- .validate_palette(palette)

  # data frame ------------------------------------------

  # ensure the variables work quoted or unquoted
  x <- ensym(x)
  y <- if (!quo_is_null(enquo(y))) ensym(y)
  type <- stats_type_switch(type)

  # one-way or two-way table?
  test <- ifelse(quo_is_null(enquo(y)), "one.way", "two.way")

  data %<>%
    select({{ x }}, {{ y }}, .counts = {{ counts }}) %>%
    tidyr::drop_na()

  # untable the data frame based on the count for each observation
  if (".counts" %in% names(data)) {
    data %<>% tidyr::uncount(weights = .counts)
  }

  # x and y need to be a factor
  data %<>% mutate(across(.cols = everything(), .fns = ~ as.factor(.x)))
  x_levels <- nlevels(pull(data, {{ x }}))
  y_levels <- ifelse(test == "one.way", 0L, nlevels(pull(data, {{ y }})))

  # one-way table not supported in `BayesFactor` ATM (richarddmorey/BayesFactor#159)
  # nocov start
  if (test == "two.way" && y_levels == 1L) {
    c(bf.message, proportion.test) %<-% c(FALSE, FALSE)
  }
  # nocov end
  if (type == "bayes" || test == "one.way") {
    proportion.test <- FALSE
  }

  # statistical analysis ------------------------------------------

  if (results.subtitle) {
    .f.args <- list(
      data = data,
      x = {{ x }},
      y = {{ y }},
      alternative = alternative,
      conf.level = conf.level,
      digits = digits,
      paired = paired,
      ratio = ratio,
      sampling.plan = sampling.plan,
      fixed.margin = fixed.margin,
      prior.concentration = prior.concentration
    )

    subtitle_df <- .eval_f(contingency_table, !!!.f.args, type = type)
    subtitle <- .extract_expression(subtitle_df)

    # Bayes Factor caption
    if (type != "bayes" && bf.message && isFALSE(paired)) {
      caption_df <- .eval_f(contingency_table, !!!.f.args, type = "bayes")
      caption <- .extract_expression(caption_df)
    }

    # pairwise comparisons
    mpc_df <- .pairwise_contingency(
      data,
      {{ x }},
      {{ y }},
      x_levels,
      y_levels,
      paired,
      digits,
      conf.level,
      alternative,
      p.adjust.method
    )
  }

  # plot ------------------------------------------

  # data frame with summary labels
  descriptive_df <- descriptive_data(data, {{ x }}, {{ y }}, label, digits.perc)

  # data frame containing all details needed for proportion test
  if (test == "two.way") {
    onesample_df <- onesample_data(data, {{ x }}, {{ y }}, digits, ratio)
  }

  # if no. of factor levels is greater than the default palette color count
  .is_palette_sufficient(palette, nlevels(pull(data, {{ x }})))

  plotBar <- ggplot(
    descriptive_df,
    if (test == "one.way") {
      aes(x = "", y = perc, fill = {{ x }})
    } else {
      aes({{ y }}, perc, fill = {{ x }})
    }
  ) +
    geom_bar(stat = "identity", position = "fill", color = "black") +
    scale_y_continuous(
      labels = ~ insight::format_percent(., digits = 0L),
      breaks = seq(from = 0.0, to = 1.0, by = 0.10),
      minor_breaks = seq(from = 0.05, to = 0.95, by = 0.10)
    ) +
    exec(
      geom_label,
      mapping = aes(label = .label, group = {{ x }}),
      position = position_fill(vjust = 0.5),
      na.rm = TRUE,
      show.legend = FALSE,
      !!!label.args
    ) +
    ggtheme +
    theme(panel.grid.major.x = element_blank()) +
    guides(fill = guide_legend(title = legend.title %||% as_name(x))) +
    paletteer::scale_fill_paletteer_d(palette, name = "", drop = FALSE)

  # proportion test ------------------------------------------

  if (isTRUE(proportion.test)) {
    plotBar <- plotBar +
      geom_text(
        data = onesample_df,
        mapping = aes(x = {{ y }}, y = 1.05, label = .p.label, fill = NULL),
        size = 2.8,
        parse = TRUE
      )
  }

  # sample size -------------------------------------------------

  if (test == "two.way") {
    plotBar <- plotBar +
      exec(
        geom_text,
        data = onesample_df,
        mapping = aes(x = {{ y }}, y = -0.05, label = N, fill = NULL),
        !!!sample.size.label.args
      )
  } else {
    plotBar <- plotBar +
      exec(
        annotate,
        geom = "text",
        x = "",
        y = -0.05,
        label = paste0("(n = ", .prettyNum(nrow(data)), ")"),
        !!!sample.size.label.args
      )
  }

  # annotations ------------------------------------------

  plotBar +
    labs(
      x = if (test == "two.way") (xlab %||% as_name(y)) else xlab,
      y = ylab,
      subtitle = subtitle,
      title = title,
      caption = caption
    ) +
    ggplot.component
}

#' @title Grouped bar charts with statistical tests
#' @name grouped_ggbarstats
#'
#' @description
#'
#' Helper function for `ggstatsplot::ggbarstats()` to apply this function across
#' multiple levels of a given factor and combining the resulting plots using
#' `ggstatsplot::combine_plots()`.
#'
#' @inheritParams ggbarstats
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams ggbarstats -title
#'
#' @seealso \code{\link{ggbarstats}}, \code{\link{ggpiestats}},
#'  \code{\link{grouped_ggpiestats}}
#'
#' @inherit ggbarstats return references
#' @inherit ggbarstats return details
#' @inherit ggbarstats return return
#'
#' @autoglobal
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' set.seed(123)
#' # grouped one-sample proportion test
#' grouped_ggbarstats(
#'   data = mtcars,
#'   x = cyl,
#'   grouping.var = am,
#'   annotation.args = list(title = "Cylinder distribution by transmission type")
#' )
#' @export
grouped_ggbarstats <- function(
  data,
  ...,
  grouping.var,
  plotgrid.args = list(),
  annotation.args = list()
) {
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
  .grouped_list(data, {{ grouping.var }}) %>%
    purrr::pmap(.f = ggbarstats, ...) %>%
    combine_plots(plotgrid.args, annotation.args)
}
