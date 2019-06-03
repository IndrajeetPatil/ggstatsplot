#' \code{ggstatsplot}
#'
#' @title ggstatsplot: 'ggplot2' Based Plots with Statistical Details
#'
#' @description `ggstatsplot` is an extension of `ggplot2` package for creating
#'   graphics with details from statistical tests included in the plots
#'   themselves and targeted primarily at behavioral sciences community to
#'   provide a one-line code to produce information-rich plots. In a typical
#'   exploratory data analysis workflow, data visualization and statistical
#'   modeling are two different phases: visualization informs modeling, and
#'   modeling in its turn can suggest a different visualization method, and so
#'   on and so forth. The central idea of ggstatsplot is simple: combine these
#'   two phases into one in the form of graphics with statistical details, which
#'   makes data exploration simpler and faster. Currently, it supports only the
#'   most common types of statistical tests (parametric, nonparametric,
#'   bayesian, and robust versions of t-test/anova, correlation, regression, and
#'   contingency tables analyses).
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
#'   multiple `ggstatsplot` plots using `cowplot::plot_grid()` with a
#'   combination of title, caption, and annotation label.
#'   \item \code{\link[ggstatsplot]{theme_ggstatsplot}} default theme used for
#'   this package.
#'  }
#'
#' For more documentation, see the dedicated
#' \href{https://indrajeetpatil.github.io/ggstatsplot/}{Website}.
#'
#' @docType package
#' @aliases ggstatsplot ggstatsplot-package
#' @name ggstatsplot-package
"_PACKAGE"
