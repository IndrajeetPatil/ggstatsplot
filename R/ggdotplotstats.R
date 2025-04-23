#' @title Dot plot/chart for labeled numeric data.
#' @name ggdotplotstats
#'
#' @description
#'
#' A dot chart (as described by William S. Cleveland) with statistical details
#' from one-sample test.
#'
#' The point estimate (and associated uncertainty) displayed depends on
#' the type of statistics selected:
#'
#'  - **mean** for parametric statistics
#'  - **median** for non-parametric statistics
#'  - **trimmed mean** for robust statistics
#'  - **MAP estimator** for Bayesian statistics
#'
#' @section Summary of graphics:
#'
#' ```{r child="man/rmd-fragments/ggdotplotstats_graphics.Rmd"}
#' ```
#'
#' @param ... Currently ignored.
#' @param y Label or grouping variable.
#' @inheritParams gghistostats
#' @inheritParams ggcoefstats
#' @inheritParams ggbetweenstats
#' @inheritParams ggcoefstats
#'
#' @inheritSection statsExpressions::one_sample_test One-sample tests
#'
#' @seealso \code{\link{grouped_gghistostats}}, \code{\link{gghistostats}},
#'  \code{\link{grouped_ggdotplotstats}}
#'
#' @autoglobal
#'
#' @details For details, see:
#' <https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggdotplotstats.html>
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' # for reproducibility
#' set.seed(123)
#'
#' # creating a plot
#' p <- ggdotplotstats(
#'   data = ggplot2::mpg,
#'   x = cty,
#'   y = manufacturer,
#'   title = "Fuel economy data",
#'   xlab = "city miles per gallon"
#' )
#'
#' # looking at the plot
#' p
#'
#' # extracting details from statistical tests
#' extract_stats(p)
#' @export
ggdotplotstats <- function(
  data,
  x,
  y,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  type = "parametric",
  test.value = 0,
  bf.prior = 0.707,
  bf.message = TRUE,
  effsize.type = "g",
  conf.int = TRUE,
  conf.level = 0.95,
  tr = 0.2,
  digits = 2L,
  results.subtitle = TRUE,
  point.args = list(color = "black", size = 3, shape = 16),
  errorbar.args = list(height = 0, na.rm = TRUE),
  centrality.plotting = TRUE,
  centrality.type = type,
  centrality.line.args = list(color = "blue", linewidth = 1, linetype = "dashed"),
  ggplot.component = NULL,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  ...
) {
  # data -----------------------------------

  # make sure both quoted and unquoted arguments are allowed
  c(x, y) %<-% c(ensym(x), ensym(y))
  type <- stats_type_switch(type)
  .f.stats.args <- list(conf.level = conf.level, digits = digits, tr = tr, bf.prior = bf.prior)

  data %<>%
    select({{ x }}, {{ y }}) %>%
    tidyr::drop_na()

  data <-
    suppressWarnings(centrality_description(
      data, {{ y }}, {{ x }},
      type = type, conf.level = conf.level, digits = digits, tr = tr, bf.prior = bf.prior
    ))

  data %<>%
    arrange({{ x }}) %>%
    mutate(
      percent_rank = percent_rank({{ x }}),
      rank = row_number()
    )

  # statistical analysis ------------------------------------------

  if (results.subtitle) {
    .f.args <- list(data = data, x = {{ x }}, test.value = test.value, effsize.type = effsize.type)

    subtitle_df <- .eval_f(one_sample_test, !!!.f.args, !!!.f.stats.args, type = type)
    subtitle <- .extract_expression(subtitle_df)

    if (type == "parametric" && bf.message) {
      caption_df <- .eval_f(one_sample_test, !!!.f.args, !!!.f.stats.args, type = "bayes")
      caption <- .extract_expression(caption_df)
    }
  }

  # plot -----------------------------------

  plot_dot <- ggplot(data, mapping = aes({{ x }}, y = rank)) +
    exec(geom_point, !!!point.args) +
    {
      if (conf.int) exec(geom_errorbarh, mapping = aes(xmin = conf.low, xmax = conf.high), !!!errorbar.args)
    } +
    scale_y_continuous(
      name = ylab,
      labels = pull(data, {{ y }}),
      breaks = data$rank,
      sec.axis = dup_axis(
        name   = "percentile",
        breaks = seq(1L, nrow(data), (nrow(data) - 1L) / 4),
        labels = 25 * 0:4
      )
    )

  # centrality plotting -------------------------------------

  if (isTRUE(centrality.plotting)) {
    plot_dot <- .histo_labeller(
      plot_dot,
      x = pull(data, {{ x }}),
      type = stats_type_switch(centrality.type),
      tr = tr,
      digits = digits,
      centrality.line.args = centrality.line.args
    )
  }

  # annotations -------------------------

  plot_dot +
    labs(
      x        = xlab %||% as_name(x),
      y        = ylab %||% as_name(y),
      title    = title,
      subtitle = subtitle,
      caption  = caption
    ) +
    ggtheme +
    ggplot.component
}


#' @title Grouped histograms for distribution of a labeled numeric variable
#' @name grouped_ggdotplotstats
#'
#' @description
#'
#' Helper function for `ggstatsplot::ggdotplotstats()` to apply this function
#' across multiple levels of a given factor and combining the resulting plots
#' using `ggstatsplot::combine_plots()`.
#'
#' @inheritParams ggdotplotstats
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams ggdotplotstats -title
#'
#' @seealso \code{\link{grouped_gghistostats}}, \code{\link{ggdotplotstats}},
#'  \code{\link{gghistostats}}
#'
#' @autoglobal
#'
#' @inherit ggdotplotstats return references
#' @inherit ggdotplotstats return details
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' # for reproducibility
#' set.seed(123)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' # removing factor level with very few no. of observations
#' df <- filter(ggplot2::mpg, cyl %in% c("4", "6", "8"))
#'
#' # plot
#' grouped_ggdotplotstats(
#'   data         = df,
#'   x            = cty,
#'   y            = manufacturer,
#'   grouping.var = cyl,
#'   test.value   = 15.5
#' )
#' @export
grouped_ggdotplotstats <- function(
  data,
  ...,
  grouping.var,
  plotgrid.args = list(),
  annotation.args = list()
) {
  .grouped_list(data, {{ grouping.var }}) %>%
    purrr::pmap(.f = ggdotplotstats, ...) %>%
    combine_plots(plotgrid.args, annotation.args)
}
