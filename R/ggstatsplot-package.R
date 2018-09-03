#' \code{ggstatsplot}
#'
#' @title ggstatsplot: 'ggplot2' Based Plots with Statistical Details
#'
#' @description ggstatsplot is an extension of ggplot2 package for creating
#'   graphics with details from statistical tests included in the plots
#'   themselves and targeted primarily at behavioral sciences community to
#'   provide a one-line code to produce information-rich plots. Currently, it
#'   supports only the most common types of statistical tests (parametric,
#'   nonparametric, and robust versions of t-test/anova, correlation, and
#'   contingency tables analyses).
#'
#' @details The main functions are- \itemize{ \item
#'   \code{\link[ggstatsplot]{ggbetweenstats}} function to produce
#'   information-rich comparison plot between different groups or conditions
#'   with `ggplot2` and details from the statistical tests in the subtitle \item
#'   \code{\link[ggstatsplot]{ggscatterstats}} function to produce `ggplot2`
#'   scatterplots along with a marginal histograms/boxplots/density plots from
#'   `ggExtra` and details from the statistical tests in the subtitle \item
#'   \code{\link[ggstatsplot]{ggpiestats}} function to produce pie chart with
#'   details from the statistical tests in the subtitle \item
#'   \code{\link[ggstatsplot]{gghistostats}} function to produce histogram for a
#'   single variable with results from one sample test displayed in the subtitle
#'   \item \code{\link[ggstatsplot]{ggcorrmat}} function to visualize
#'   correlation matrix \item \code{\link[ggstatsplot]{combine_plots}} helper
#'   function to combine multiple `ggstatsplot` plots using
#'   `cowplot::plot_grid()` with a combination of title, caption, and annotation
#'   label \item \code{\link[ggstatsplot]{ggcoefstats}} function to visualize
#'   results from regression analyses \item
#'   \code{\link[ggstatsplot]{theme_ggstatsplot}} default theme used for this
#'   package \item \code{\link[ggstatsplot]{specify_decimal_p}} helper function
#'   to format results for pretty printing }
#'
#' For more documentation, see the dedicated
#' \href{https://indrajeetpatil.github.io/ggstatsplot/}{Website}.
#'
#' @docType package
#' @name ggstatsplot-package
#'
NULL
