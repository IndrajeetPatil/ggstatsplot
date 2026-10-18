#' @title Extracting data frames or expressions from `{ggstatsplot}` plots
#'
#' @details
#'
#' These helpers retrieve the statistical data frames and expressions retained
#' by `{ggstatsplot}` plots. If you only need tabular results, call the
#' underlying analysis functions directly: `{statsExpressions}` for statistical
#' tests, [`parameters::model_parameters()`] for model coefficients, and
#' [`performance::model_performance()`] for model fit indices.
#'
#' `ggcorrmat()` does not retain statistical data frames, so all elements
#' returned by `extract_stats()` are `NULL`. Use
#' [`correlation::correlation()`] to obtain its statistical results as data.
#'
#' @returns
#'
#' For `extract_stats()`, a list with the following elements, each either a
#' data frame or `NULL` if not relevant for the given plot:
#'
#' - `subtitle_data`: results of the test displayed in the subtitle
#' - `caption_data`: results of the test displayed in the caption (typically,
#'   the Bayesian test)
#' - `pairwise_comparisons_data`: results of pairwise comparisons
#' - `descriptive_data`: counts and percentages (`ggpiestats()` and
#'   `ggbarstats()`)
#' - `one_sample_data`: proportion tests for each level of `y`
#'   (`ggpiestats()` and `ggbarstats()`)
#' - `tidy_data`: tidy model parameters (`ggcoefstats()`)
#' - `glance_data`: model performance indices (`ggcoefstats()`)
#'
#' For `extract_subtitle()` and `extract_caption()`, the statistical expression
#' used in the subtitle or caption, respectively (or `NULL` if unavailable).
#' User-supplied subtitle and caption text is not returned.
#'
#' For plots from `grouped_*` functions, a list with one such element per plot.
#'
#' @param p A plot from `{ggstatsplot}`.
#'
#' @autoglobal
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' set.seed(123)
#'
#' # non-grouped plot
#' p1 <- ggbetweenstats(mtcars, cyl, mpg)
#'
#' # grouped plot
#' p2 <- grouped_ggbarstats(Titanic_full, Survived, Sex, grouping.var = Age)
#'
#' # extracting expressions -----------------------------
#'
#' extract_subtitle(p1)
#' extract_caption(p1)
#'
#' extract_subtitle(p2)
#' extract_caption(p2)
#'
#' # extracting data frames -----------------------------
#'
#' extract_stats(p1)
#'
#' extract_stats(p2)
#' @export
extract_stats <- function(p) .map_plots(p, .extract_stats)

.map_plots <- function(p, .f) {
  if (inherits(p, "patchwork")) {
    purrr::map(as.list(p), .f)
  } else {
    .f(p)
  }
}

# Statistical data frames are either bound directly in the plot environment or
# kept in the `stats_output` list returned by the subtitle/caption helpers.
.pluck_plot_env <- function(p, data) {
  purrr::pluck(p, "plot_env", data) %||%
    purrr::pluck(p, "plot_env", "stats_output", data)
}

.extract_stats <- function(p) {
  structure(
    list(
      subtitle_data = .pluck_plot_env(p, "subtitle_df"),
      caption_data = .pluck_plot_env(p, "caption_df"),
      pairwise_comparisons_data = .pluck_plot_env(p, "mpc_df"),
      descriptive_data = .pluck_plot_env(p, "descriptive_df"),
      one_sample_data = .pluck_plot_env(p, "onesample_df"),
      tidy_data = .pluck_plot_env(p, "tidy_df"),
      glance_data = .pluck_plot_env(p, "glance_df")
    ),
    class = c("ggstatsplot_stats", "list")
  )
}


#' @rdname extract_stats
#' @export
extract_subtitle <- function(p) .extract_plot_expression(p, "subtitle_df")

#' @rdname extract_stats
#' @export
extract_caption <- function(p) .extract_plot_expression(p, "caption_df")

.extract_plot_expression <- function(p, data) {
  .map_plots(p, \(x) .extract_expression(.pluck_plot_env(x, data)))
}
