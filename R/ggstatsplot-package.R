#' \code{ggstatsplot}
#'
#' @title ggstatsplot: 'ggplot2' Based Plots with Statistical Details
#'
#' @description `ggstatsplot` is an extension of 'ggplot2', 'ggstatsplot'
#'   creates graphics with details from statistical tests included in the plots
#'   themselves. It provides an easier API to generate information-rich plots
#'   for statistical analysis of continuous (violin plots, scatterplots,
#'   histograms, dot plots, dot-and-whisker plots) or categorical (pie and bar
#'   charts) data. Currently, it supports the most common types of statistical
#'   tests: parametric, nonparametric, robust, and Bayesian versions of
#'   t-test/ANOVA, correlation analyses, contingency table analysis,
#'   meta-analysis, and regression analyses.
#'
#' @details The main functions are-
#' \itemize{
#'   \item \code{\link[ggstatsplot]{ggbetweenstats}} function to produce
#'   information-rich comparison plot *between* different groups or conditions
#'   with `ggplot2` and details from the statistical tests in the subtitle.
#'   \item \code{\link[ggstatsplot]{ggwithinstats}} function to produce
#'   information-rich comparison plot *within* different groups or conditions
#'   with `ggplot2` and details from the statistical tests in the subtitle.
#'   \item \code{\link[ggstatsplot]{ggscatterstats}} function to produce
#'   `ggplot2` scatterplots along with a marginal histograms/boxplots/density
#'   plots from `ggExtra` and details from the statistical tests in the
#'   subtitle.
#'   \item \code{\link[ggstatsplot]{ggpiestats}} function to produce pie chart
#'   with details from the statistical tests in the subtitle.
#'   \item \code{\link[ggstatsplot]{ggbarstats}} function to produce stacked bar
#'   chart with details from the statistical tests in the subtitle.
#'   \item \code{\link[ggstatsplot]{gghistostats}} function to produce histogram
#'   for a single variable with results from one sample test displayed in the
#'   subtitle.
#'   \item \code{\link[ggstatsplot]{ggdotplotstats}} function to produce
#'   Cleveland-style dot plots/charts for a single variable with labels and
#'   results from one sample test displayed in the subtitle.
#'   \item \code{\link[ggstatsplot]{ggcorrmat}} function to visualize the
#'   correlation matrix.
#'   \item \code{\link[ggstatsplot]{ggcoefstats}} function to visualize
#'   results from regression analyses.
#'   \item \code{\link[ggstatsplot]{combine_plots}} helper function to combine
#'   multiple `ggstatsplot` plots using `patchwork::wrap_plots()` with a
#'   combination of title, caption, and annotation label.
#'  }
#'
#' For more documentation, see the dedicated
#' \href{https://indrajeetpatil.github.io/ggstatsplot/}{Website}.
#'
#' @docType package
#' @aliases ggstatsplot ggstatsplot-package
#' @name ggstatsplot-package
"_PACKAGE"
