#' @title Pie charts with statistical tests
#' @name ggpiestats
#'
#' @description
#' Pie charts for categorical data with statistical details included in the plot
#' as a subtitle.
#'
#' @section Summary of graphics:
#'
#' ```{r child="man/rmd-fragments/ggpiestats_graphics.Rmd"}
#' ```
#'
#' @param x The variable to use as the **rows** in the contingency table. Please
#'   note that if there are empty factor levels in your variable, they will be
#'   dropped.
#' @param y The variable to use as the **columns** in the contingency table.
#'   Please note that if there are empty factor levels in your variable, they
#'   will be dropped. Default is `NULL`. If `NULL`, one-sample proportion test
#'   (a goodness of fit test) will be run for the `x` variable. Otherwise an
#'   appropriate association test will be run.
#' @param proportion.test Decides whether proportion test for `x` variable is to
#'   be carried out for each level of `y`. Defaults to `results.subtitle`. In
#'   [`ggbarstats()`], only *p*-values from this test will be displayed.
#' @param digits.perc Numeric that decides number of decimal places for
#'   percentage labels (Default: `0L`).
#' @param label Character decides what information needs to be displayed
#'   on the label in each pie slice. Possible options are `"percentage"`
#'   (default), `"counts"`, `"both"`.
#' @param label.args Additional aesthetic arguments that will be passed to
#'   [`ggplot2::geom_label()`].
#' @param label.repel Whether labels should be repelled using `{ggrepel}`
#'   package. This can be helpful in case of overlapping labels.
#' @param legend.title Title text for the legend.
#' @inheritParams ggbetweenstats
#' @inheritParams statsExpressions::contingency_table
#' @inheritParams theme_ggstatsplot
#'
#' @inheritSection statsExpressions::contingency_table Contingency table analyses
#'
#' @section Pairwise comparisons:
#' When there is a two-way table and `x` has more than two levels, pairwise
#' contingency table analyses (Fisher's exact tests) are computed using
#' [pairwise_contingency_table()]. These pairwise results are **not**
#' displayed in the plot because bar and pie charts lack a natural visual
#' representation for pairwise significance annotations (unlike box/violin
#' plots, which use bracket annotations). Additionally, there is no
#' established convention for overlaying pairwise comparisons on pie charts,
#' and both `ggpiestats()` and `ggbarstats()` are designed to remain visually
#' congruent. The pairwise results are available as a data frame via
#' `extract_stats(plot)$pairwise_comparisons_data`.
#'
#' @seealso \code{\link{grouped_ggpiestats}}, \code{\link{ggbarstats}},
#'  \code{\link{grouped_ggbarstats}}
#'
#' @autoglobal
#'
#' @details For details, see:
#' <https://www.indrapatil.com/ggstatsplot/articles/web_only/ggpiestats.html>
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' # for reproducibility
#' set.seed(123)
#'
#' # one sample goodness of fit proportion test
#' p <- ggpiestats(mtcars, vs)
#'
#' # looking at the plot
#' p
#'
#' # extracting details from statistical tests
#' extract_stats(p)
#'
#' # association test (or contingency table analysis)
#' ggpiestats(mtcars, vs, cyl)
#'
#' # Bayesian test
#' ggpiestats(mtcars, vs, cyl, type = "bayes")
#'
#' # with repelled labels to avoid overlapping
#' ggpiestats(mtcars, vs, label.repel = TRUE)
#'
#' # show counts instead of percentages
#' ggpiestats(mtcars, vs, label = "counts")
#'
#' # show both counts and percentages
#' ggpiestats(mtcars, vs, label = "both")
#'
#' # using pre-aggregated data with counts
#' ggpiestats(as.data.frame(Titanic), Survived, counts = Freq)
#'
#' @export
ggpiestats <- function(
  data,
  x,
  y = NULL,
  counts = NULL,
  type = "parametric",
  paired = FALSE,
  results.subtitle = TRUE,
  label = "percentage",
  label.args = list(direction = "both"),
  label.repel = FALSE,
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
  prior.concentration = 1,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  legend.title = NULL,
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
    bf.message <- FALSE
  }
  # nocov end

  facet <- as.logical(y_levels > 1L)
  if ((x_levels == 1L && facet) || type == "bayes") {
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
      sampling.plan = sampling.plan,
      fixed.margin = fixed.margin,
      prior.concentration = prior.concentration,
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
  .is_palette_sufficient(palette, min_length = x_levels)

  # creating the basic plot
  plotPie <- ggplot(descriptive_df, mapping = aes(x = "", y = perc)) +
    geom_col(
      mapping = aes(fill = {{ x }}),
      position = "fill",
      color = "black",
      width = 1.0
    )

  # whether labels need to be repelled
  .fn <- if (label.repel) ggrepel::geom_label_repel else ggplot2::geom_label

  # adding label with percentages and/or counts
  suppressWarnings(suppressMessages({
    plotPie <- plotPie +
      exec(
        .fn,
        mapping = aes(label = .label, group = {{ x }}),
        position = position_fill(vjust = 0.5),
        min.segment.length = 0,
        fill = "white",
        alpha = 1.0,
        na.rm = TRUE,
        show.legend = FALSE,
        !!!label.args
      )
  }))

  # if facet_wrap *is* happening
  if (facet) {
    plotPie <- plotPie + facet_wrap(facets = vars({{ y }}))
  }

  # polar coordinates plus formatting
  plotPie <- plotPie +
    coord_polar(theta = "y") +
    scale_y_continuous(breaks = NULL) +
    paletteer::scale_fill_paletteer_d(palette, name = "", drop = FALSE) +
    ggtheme +
    theme(panel.grid = element_blank(), axis.ticks = element_blank()) +
    guides(fill = guide_legend(override.aes = list(color = NA)))

  # sample size + proportion test ------------------------------------------

  if (facet && proportion.test) {
    plotPie <- plotPie +
      exec(
        geom_text,
        data = onesample_df,
        mapping = aes(label = .label, x = 1.65, y = 0.5),
        position = position_fill(vjust = 1.0),
        size = 2.8,
        parse = TRUE
      )
  }

  # annotations ------------------------------------------

  plotPie +
    labs(
      x = NULL,
      y = NULL,
      subtitle = subtitle,
      title = title,
      caption = caption
    ) +
    guides(fill = guide_legend(title = legend.title %||% as_name(x))) +
    ggplot.component
}


#' @title Grouped pie charts with statistical tests
#' @name grouped_ggpiestats
#' @description Helper function for `ggstatsplot::ggpiestats` to apply this
#'   function across multiple levels of a given factor and combining the
#'   resulting plots using `ggstatsplot::combine_plots`.
#'
#' @inheritParams ggpiestats
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams ggpiestats -title
#'
#' @seealso \code{\link{ggbarstats}}, \code{\link{ggpiestats}},
#'  \code{\link{grouped_ggbarstats}}
#'
#' @autoglobal
#'
#' @inherit ggpiestats return references
#' @inherit ggpiestats return details
#' @inherit ggpiestats return return
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' set.seed(123)
#' # grouped one-sample proportion test
#' grouped_ggpiestats(
#'   data = mtcars,
#'   x = cyl,
#'   grouping.var = am,
#'   annotation.args = list(title = "Cylinder distribution by transmission type")
#' )
#' @export
grouped_ggpiestats <- .make_grouped_fn(ggpiestats, .pre = .stabilize_x_factor)
