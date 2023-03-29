#' \code{ggstatsplot}
#'
#' @title ggstatsplot: 'ggplot2' Based Plots with Statistical Details
#'
#' @description
#'
#' `{ggstatsplot}` is an extension of `{ggplot2}` package. It creates
#'   graphics with details from statistical tests included in the plots
#'   themselves. It provides an easier `API` to generate information-rich plots
#'   for statistical analysis of continuous (violin plots, scatterplots,
#'   histograms, dot plots, dot-and-whisker plots) or categorical (pie and bar
#'   charts) data. Currently, it supports the most common types of statistical
#'   tests: parametric, nonparametric, robust, and Bayesian versions of
#'   *t*-test/ANOVA, correlation analyses, contingency table analysis,
#'   meta-analysis, and regression analyses.
#'
#' @details
#'
#' The main functions are:
#'
#'   - `ggbetweenstats()` function to produce information-rich comparison plot
#'   *between* different groups or conditions with `{ggplot2}` and details from
#'   the statistical tests in the subtitle.
#'
#'   - `ggwithinstats()` function to produce information-rich comparison plot
#'   *within* different groups or conditions with `{ggplot2}` and details from
#'   the statistical tests in the subtitle.
#'
#'   - `ggscatterstats()` function to produce `{ggplot2}` scatterplots along
#'   with a marginal distribution plots from `{ggside}` package and details from
#'   the statistical tests in the subtitle.
#'
#'   - `ggpiestats()` function to produce pie chart with details from the
#'   statistical tests in the subtitle.
#'
#'   - `ggbarstats()` function to produce stacked bar chart with details from
#'   the statistical tests in the subtitle.
#'
#'   - `gghistostats()` function to produce histogram for a single variable with
#'   results from one sample test displayed in the subtitle.
#'
#'   - `ggdotplotstats()` function to produce Cleveland-style dot plots/charts
#'   for a single variable with labels and results from one sample test
#'   displayed in the subtitle.
#'
#'   - `ggcorrmat()` function to visualize the correlation matrix.
#'
#'   - `ggcoefstats()` function to visualize results from regression analyses.
#'
#'   - `combine_plots()` helper function to combine multiple `{ggstatsplot}`
#'   plots using `patchwork::wrap_plots()`.
#'
#' References: Patil (2021) \doi{10.21105/joss.03236}.
#'
#' For more documentation, see the dedicated
#' \href{https://indrajeetpatil.github.io/ggstatsplot/}{Website}.
#'
#' @docType package
#' @keywords internal
#' @aliases ggstatsplot ggstatsplot-package
#' @name ggstatsplot-package
"_PACKAGE"

## ggstatsplot namespace: start
#'
#' @import rlang
#' @import dplyr
#' @import ggplot2
#' @import statsExpressions
#'
#' @importFrom glue glue
#' @importFrom insight is_model find_statistic format_value check_if_installed
#' @importFrom parameters model_parameters standardize_names
#' @importFrom paletteer scale_color_paletteer_d scale_fill_paletteer_d
#' @importFrom purrr pmap map
#' @importFrom correlation correlation
#' @importFrom patchwork wrap_plots plot_annotation
#'
## ggstatsplot namespace: end
NULL
