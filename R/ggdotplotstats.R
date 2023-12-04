#' @title Dot plot/chart for labeled numeric data.
#' @name ggdotplotstats
#'
#' @description
#'
#' A dot chart (as described by William S. Cleveland) with statistical details
#' from one-sample test.
#'
#' @section Summary of graphics:
#'
#' ```{r child="man/rmd-fragments/gghistostats_graphics.Rmd"}
#' ```
#'
#' @param ... Currently ignored.
#' @param y Label or grouping variable.
#' @param point.args A list of additional aesthetic arguments passed to
#'   `geom_point`.
#' @inheritParams gghistostats
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
    conf.level = 0.95,
    tr = 0.2,
    k = 2L,
    results.subtitle = TRUE,
    point.args = list(color = "black", size = 3, shape = 16),
    centrality.plotting = TRUE,
    centrality.type = type,
    centrality.line.args = list(color = "blue", linewidth = 1, linetype = "dashed"),
    ggplot.component = NULL,
    ggtheme = ggstatsplot::theme_ggstatsplot(),
    ...) {
  # data -----------------------------------


  type <- stats_type_switch(type)

  # ensure the variables work quoted or unquoted
  c(x, y) %<-% c(ensym(x), ensym(y))

  data %<>%
    select({{ x }}, {{ y }}) %>%
    tidyr::drop_na() %>%
    mutate({{ y }} := droplevels(as.factor({{ y }}))) %>%
    summarise({{ x }} := mean({{ x }}), .by = {{ y }}) %>%
    # rank ordering the data
    arrange({{ x }}) %>%
    mutate(
      percent_rank = percent_rank({{ x }}),
      rank = row_number()
    )

  # statistical analysis ------------------------------------------

  if (results.subtitle) {
    .f.args <- list(
      data         = data,
      x            = {{ x }},
      test.value   = test.value,
      effsize.type = effsize.type,
      conf.level   = conf.level,
      k            = k,
      tr           = tr,
      bf.prior     = bf.prior
    )

    subtitle_df <- .eval_f(one_sample_test, !!!.f.args, type = type)
    subtitle <- .extract_expression(subtitle_df)

    if (type == "parametric" && bf.message) {
      caption_df <- .eval_f(one_sample_test, !!!.f.args, type = "bayes")
      caption <- .extract_expression(caption_df)
    }
  }

  # plot -----------------------------------

  plot <- ggplot(data, mapping = aes({{ x }}, y = rank)) +
    exec(geom_point, !!!point.args) +
    scale_y_continuous(
      name = ylab,
      labels = pull(data, {{ y }}),
      breaks = data$rank,
      sec.axis = dup_axis(
        name   = "percentile",
        breaks = seq(1, nrow(data), (nrow(data) - 1) / 4),
        labels = 25 * 0:4
      )
    )

  # centrality plotting -------------------------------------

  if (isTRUE(centrality.plotting)) {
    plot <- .histo_labeller(
      plot,
      x                    = pull(data, {{ x }}),
      type                 = stats_type_switch(centrality.type),
      tr                   = tr,
      k                    = k,
      centrality.line.args = centrality.line.args
    )
  }

  # annotations -------------------------

  plot +
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
#' Helper function for `ggstatsplot::ggdotplotstats` to apply this function
#' across multiple levels of a given factor and combining the resulting plots
#' using `ggstatsplot::combine_plots`.
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
    annotation.args = list()) {
  .grouped_list(data, {{ grouping.var }}) %>%
    purrr::pmap(.f = ggdotplotstats, ...) %>%
    combine_plots(plotgrid.args, annotation.args)
}
