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

  x <- ensym(x)
  y <- if (!quo_is_null(enquo(y))) ensym(y)
  type <- stats_type_switch(type)

  prep <- .pie_bar_data_prep(
    data = data,
    x = {{ x }},
    y = {{ y }},
    counts = {{ counts }}
  )
  data <- prep$data
  test <- prep$test
  x_levels <- prep$x_levels
  y_levels <- prep$y_levels

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
    stats_output <- .pie_bar_subtitle_caption(
      data = data,
      x = {{ x }},
      y = {{ y }},
      type = type,
      paired = paired,
      bf.message = bf.message,
      alternative = alternative,
      conf.level = conf.level,
      digits = digits,
      ratio = ratio,
      sampling.plan = "indepMulti",
      fixed.margin = "rows",
      prior.concentration = 1,
      x_levels = x_levels,
      y_levels = y_levels,
      p.adjust.method = p.adjust.method
    )
    subtitle <- stats_output$subtitle
    caption <- stats_output$caption %||% caption
    subtitle_df <- stats_output$subtitle_df
    caption_df <- stats_output$caption_df
    mpc_df <- stats_output$mpc_df
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
grouped_ggbarstats <- .make_grouped_fn(ggbarstats, .pre = .stabilize_x_factor)
