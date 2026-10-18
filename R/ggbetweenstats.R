#' @title Box/Violin plots for between-subjects comparisons
#' @name ggbetweenstats
#'
#' @description
#'
#' A combination of box and violin plots along with jittered data points for
#' between-subjects designs with statistical details included in the plot as a
#' subtitle.
#'
#' @section Summary of graphics:
#'
#' ```{r child="man/rmd-fragments/ggbetweenstats_graphics.Rmd"}
#' ```
#'
#' @param xlab Label for `x` axis variable. If `NULL` (default),
#'   variable name for `x` will be used.
#' @param ylab Label for `y` axis variable. If `NULL` (default),
#'   variable name for `y` will be used.
#' @param p.adjust.method Adjustment method for *p*-values for multiple
#'   comparisons. Possible methods are: `"holm"` (default), `"hochberg"`,
#'   `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`.
#' @param pairwise.display Decides *which* pairwise comparisons to display.
#'   Available options are:
#'   - `"significant"` (abbreviation accepted: `"s"`)
#'   - `"non-significant"` (abbreviation accepted: `"ns"`)
#'   - `"all"`
#'   - `"none"`
#'
#'   Use this to keep the plot uncluttered when many groups are compared.
#'   Pairwise comparisons are carried out (and displayed) only when `x` has
#'   three or more levels. For `type = "bayes"`, there are no *p*-values, so all
#'   comparisons are displayed unless this is `"none"`.
#' @param pairwise.alpha Numeric alpha threshold used to decide which pairwise
#'   comparisons are displayed when `pairwise.display = "significant"` or
#'   `pairwise.display = "non-significant"` (Default: `0.05`). It is compared
#'   against *p*-values adjusted using `p.adjust.method`.
#' @param bf.message Logical that decides whether to display the Bayes Factor in
#'   favor of the *null* hypothesis as a caption. Relevant only for parametric
#'   tests (Default: `TRUE`).
#' @param results.subtitle Logical that decides whether the results of
#'   statistical tests are displayed as a subtitle (Default: `TRUE`). Setting
#'   this to `FALSE` also skips the automatic Bayes Factor caption. Other plot
#'   details have their own arguments.
#' @param title The text for the plot title.
#' @param subtitle The text for the plot subtitle. Used only if
#'   `results.subtitle = FALSE`.
#' @param caption The text for the plot caption. It will be replaced by the
#'   Bayes Factor caption whenever that is displayed (see `bf.message`).
#' @param centrality.plotting Logical that decides whether central tendency
#'   measure is to be displayed as a point with a label (Default: `TRUE`).
#'   Function decides which central tendency measure to show depending on the
#'   `type` argument.
#'   - **mean** for parametric statistics
#'   - **median** for non-parametric statistics
#'   - **trimmed mean** for robust statistics
#'   - **MAP estimator** for Bayesian statistics
#'
#'   Use `centrality.type` to choose a different centrality measure.
#' @param centrality.type Decides which centrality measure is displayed. Defaults
#'   to the same value as `type`. Can be one of:
#'   - `"parametric"` (for **mean**)
#'   - `"nonparametric"` (for **median**)
#'   - `"robust"` (for **trimmed mean**)
#'   - `"bayes"` (for **MAP estimator**)
#'
#'   As with `type`, abbreviations are accepted.
#' @param point.args A list of additional aesthetic arguments passed to
#'   [`ggplot2::geom_point()`].
#' @param boxplot.args A list of additional aesthetic arguments passed to
#'   [`ggplot2::geom_boxplot()`]. By default, the whiskers extend to 1.5 times
#'   the interquartile range (IQR) from the box (Tukey-style). Use `coef` to
#'   change this, e.g., `boxplot.args = list(coef = 3)` extends the whiskers to
#'   the most extreme observations within 3 * IQR of the hinges.
#' @param violin.args A list of additional aesthetic arguments passed to
#'   [`ggplot2::geom_violin()`].
#' @param ggplot.component A `{ggplot2}` component, or a list of components, to
#'   add to the plot (Default: `NULL`). This is mostly helpful for the
#'   `grouped_` variants, where it is applied to each individual plot.
#' @param palette Name of the discrete palette in `"package::palette"` format
#'   to be used for coloring (via `{paletteer}`). Run
#'   `View(paletteer::palettes_d_names)` to see all available options.
#' @param ... Currently ignored.
#' @param centrality.point.args,centrality.label.args A list of additional
#'   aesthetic arguments passed to [`ggplot2::geom_point()`] and
#'   [`ggrepel::geom_label_repel()`], respectively, which display the
#'   centrality measure.
#' @param ggsignif.args A list of additional aesthetic arguments passed to
#'   [`ggsignif::geom_signif()`].
#' @param ggtheme A `{ggplot2}` theme (Default: [`theme_ggstatsplot()`]). Any
#'   `{ggplot2}` theme (e.g., [`ggplot2::theme_bw()`]) or theme from an
#'   extension package (e.g., `ggthemes::theme_fivethirtyeight()`) can be used.
#'   Note that some themes remove plot details. For example,
#'   `ggthemes::theme_fivethirtyeight()` removes the secondary Y-axis, which
#'   [`ggbetweenstats()`] uses to show details about pairwise comparisons.
#' @param x The grouping (or independent) variable from `data`.
#' @inheritParams statsExpressions::oneway_anova
#' @inheritParams statsExpressions::two_sample_test
#'
#' @section Statistical defaults:
#'
#' This function uses statistically justified defaults that are not
#' user-configurable:
#'
#' - For **parametric tests**, effect sizes use Hedges' *g* for two groups
#'   and omega-squared for three or more groups. These estimators correct for
#'   positive bias, which can be especially relevant in small samples.
#'
#' - **Welch's *t*-test and one-way test** are used instead of Student's
#'   versions, so equal variances are not assumed.
#'
#' Users who need non-default values for these settings can call
#' `{statsExpressions}` directly.
#'
#' @inheritSection statsExpressions::centrality_description Centrality measures
#' @inheritSection statsExpressions::two_sample_test Two-sample tests
#' @inheritSection statsExpressions::oneway_anova One-way ANOVA
#' @inheritSection statsExpressions::pairwise_comparisons Pairwise comparison tests
#'
#' @seealso [grouped_ggbetweenstats()], [ggwithinstats()],
#'   [grouped_ggwithinstats()]
#'
#' @autoglobal
#'
#' @returns A `ggplot` object, which can be further modified with `{ggplot2}`
#'   functions. Use [`extract_stats()`] to get the statistical details shown in
#'   the plot as data frames.
#'
#' @details Rows with a missing value in either `x` or `y` are removed before
#'   plotting and analysis, and unused levels of `x` are dropped.
#'
#' For details, see:
#' <https://www.indrapatil.com/ggstatsplot/articles/web_only/ggbetweenstats.html>
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' # for reproducibility
#' set.seed(123)
#'
#' p <- ggbetweenstats(mtcars, am, mpg)
#' p
#'
#' # extracting details from statistical tests
#' extract_stats(p)
#'
#' # show non-significant pairwise comparisons (needs 3+ groups for ggsignif)
#' ggbetweenstats(mtcars, cyl, mpg, pairwise.display = "non-significant")
#'
#' # show all pairwise comparisons
#' ggbetweenstats(mtcars, cyl, mpg, pairwise.display = "all")
#'
#' # use a stricter alpha threshold for significant pairwise comparisons
#' ggbetweenstats(mtcars, cyl, mpg, pairwise.alpha = 0.001)
#'
#' # modifying defaults
#' ggbetweenstats(
#'   morley,
#'   x    = Expt,
#'   y    = Speed,
#'   type = "robust",
#'   xlab = "The experiment number",
#'   ylab = "Speed-of-light measurement"
#' )
#'
#' # you can remove a specific geom to reduce complexity of the plot
#' ggbetweenstats(
#'   mtcars,
#'   am,
#'   wt,
#'   # to remove violin plot
#'   violin.args = list(width = 0, linewidth = 0, colour = NA),
#'   # to remove boxplot
#'   boxplot.args = list(width = 0),
#'   # to remove points
#'   point.args = list(alpha = 0)
#' )
#' @export
ggbetweenstats <- function(
  data,
  x,
  y,
  type = "parametric",
  pairwise.display = "significant",
  pairwise.alpha = 0.05,
  p.adjust.method = "holm",
  bf.prior = 0.707,
  bf.message = TRUE,
  results.subtitle = TRUE,
  xlab = NULL,
  ylab = NULL,
  caption = NULL,
  title = NULL,
  subtitle = NULL,
  digits = 2L,
  conf.level = 0.95,
  tr = 0.2,
  alternative = "two.sided",
  centrality.plotting = TRUE,
  centrality.type = type,
  centrality.point.args = list(size = 5, color = "darkred"),
  centrality.label.args = list(
    size = 3,
    nudge_x = 0.4,
    segment.linetype = 4,
    min.segment.length = 0
  ),
  point.args = list(
    position = ggplot2::position_jitterdodge(dodge.width = 0.60),
    alpha = 0.4,
    size = 3,
    stroke = 0,
    na.rm = TRUE
  ),
  boxplot.args = list(width = 0.3, alpha = 0.2, na.rm = TRUE),
  violin.args = list(width = 0.5, alpha = 0.2, na.rm = TRUE),
  ggsignif.args = list(textsize = 3, tip_length = 0.01, na.rm = TRUE),
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  palette = "ggthemes::gdoc",
  ggplot.component = NULL,
  ...
) {
  palette <- .validate_palette(palette)

  # data -----------------------------------

  # make sure both quoted and unquoted arguments are allowed
  x <- ensym(x)
  y <- ensym(y)
  type <- extract_stats_type(type)

  data <- .prep_data(data, {{ x }}, {{ y }}, x_as_factor = TRUE)

  # statistical analysis ------------------------------------------

  test <- if (nlevels(pull(data, {{ x }})) < 3L) "t" else "anova"

  if (results.subtitle) {
    stats_output <- .bw_subtitle_caption(
      data = data,
      x = x,
      y = y,
      test = test,
      type = type,
      bf.message = bf.message,
      bf.prior = bf.prior,
      conf.level = conf.level,
      digits = digits,
      tr = tr,
      alternative = alternative,
      paired = FALSE
    )
    subtitle <- stats_output$subtitle
    caption <- stats_output$caption %||% caption
  }

  # plot -----------------------------------

  plot_comparison <- ggplot(data, mapping = aes({{ x }}, {{ y }})) +
    exec(geom_point, aes(color = {{ x }}), !!!point.args) +
    exec(geom_boxplot, !!!boxplot.args, outlier.shape = NA) +
    exec(geom_violin, !!!violin.args)

  # decorate and return -----------------------------------

  .bw_decorate(
    plot = plot_comparison,
    data = data,
    x = x,
    y = y,
    type = type,
    test = test,
    centrality.plotting = centrality.plotting,
    centrality.type = centrality.type,
    digits = digits,
    tr = tr,
    centrality.point.args = centrality.point.args,
    centrality.label.args = centrality.label.args,
    pairwise.display = pairwise.display,
    pairwise.alpha = pairwise.alpha,
    pairwise_args = list(
      paired = FALSE,
      p.adjust.method = p.adjust.method
    ),
    ggsignif.args = ggsignif.args,
    xlab = xlab,
    ylab = ylab,
    title = title,
    subtitle = subtitle,
    caption = caption,
    ggtheme = ggtheme,
    palette = palette,
    ggplot.component = ggplot.component
  )
}


#' @title Grouped box/violin plots for between-subjects comparisons
#' @name grouped_ggbetweenstats
#'
#' @description
#'
#' Applies [`ggbetweenstats()`] across all levels of `grouping.var` and combines
#' the resulting plots using [`combine_plots()`].
#'
#' @inheritParams ggbetweenstats
#' @inheritParams .grouped_list
#' @inheritParams combine_plots
#' @inheritDotParams ggbetweenstats -title
#'
#' @autoglobal
#'
#' @seealso [ggbetweenstats()], [ggwithinstats()], [grouped_ggwithinstats()]
#'
#' @returns A `patchwork` object combining one plot per level of
#'   `grouping.var` (see [`combine_plots()`]). Applying [`extract_stats()`] to
#'   it returns a list with the statistical details for each of these plots.
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' # for reproducibility
#' set.seed(123)
#'
#' library(dplyr, warn.conflicts = FALSE)
#' library(ggplot2)
#'
#' grouped_ggbetweenstats(
#'   data = filter(ggplot2::mpg, drv != "4"),
#'   x = year,
#'   y = hwy,
#'   grouping.var = drv
#' )
#'
#' # modifying individual plots using `ggplot.component` argument
#' grouped_ggbetweenstats(
#'   data = filter(
#'     movies_long,
#'     genre %in% c("Action", "Comedy"),
#'     mpaa %in% c("R", "PG")
#'   ),
#'   x = genre,
#'   y = rating,
#'   grouping.var = mpaa,
#'   ggplot.component = scale_y_continuous(
#'     breaks = seq(1, 9, 1),
#'     limits = c(1, 9)
#'   ),
#'   annotation.args = list(title = "Ratings by genre for different MPAA ratings")
#' )
#' @export
grouped_ggbetweenstats <- .make_grouped_fn(ggbetweenstats)
