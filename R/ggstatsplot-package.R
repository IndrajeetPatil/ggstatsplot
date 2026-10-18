#' @title ggstatsplot: 'ggplot2' Based Plots with Statistical Details
#'
#' @description
#'
#' `{ggstatsplot}` is an extension of `{ggplot2}` that creates graphics with
#' details from statistical tests included in the plots themselves. It provides
#' an easier API to generate information-rich plots for statistical analysis of
#' continuous (box/violin plots, scatterplots, histograms, dot plots,
#' dot-and-whisker plots) or categorical (pie and bar charts) data. It supports
#' the most common types of statistical tests: parametric, nonparametric,
#' robust, and Bayesian versions of *t*-test/ANOVA, correlation analyses,
#' contingency table analysis, meta-analysis, and regression analyses.
#'
#' @details
#'
#' The main functions are:
#'
#'   - [`ggbetweenstats()`]: box/violin plots for comparisons *between* groups
#'     or conditions.
#'   - [`ggwithinstats()`]: box/violin plots for comparisons *within* groups or
#'     conditions (repeated measures).
#'   - [`ggscatterstats()`]: scatterplots with marginal distributions from
#'     `{ggside}`.
#'   - [`ggpiestats()`] and [`ggbarstats()`]: pie charts and stacked bar charts
#'     for categorical data.
#'   - [`gghistostats()`]: histograms for a single numeric variable, with
#'     results from a one-sample test.
#'   - [`ggdotplotstats()`]: Cleveland-style dot plots for labeled numeric data,
#'     with results from a one-sample test.
#'   - [`ggcorrmat()`]: correlation matrices.
#'   - [`ggcoefstats()`]: dot-and-whisker plots for regression analyses.
#'
#' All of them except `ggcoefstats()` have `grouped_` variants that repeat the
#' analysis across the levels of a grouping variable and combine the plots with
#' [`combine_plots()`].
#'
#' References: Patil (2021) \doi{10.21105/joss.03167}.
#'
#' For more documentation, see the dedicated
#' \href{https://www.indrapatil.com/ggstatsplot/}{Website}.
#'
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
#' @import insight
#'
#' @importFrom glue glue
#'
## ggstatsplot namespace: end
NULL
