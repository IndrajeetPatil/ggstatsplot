#' @title Extracting dataframes with statistical details from `{ggstatsplot}`
#'
#' @details
#'
#' This is a convenience function to extract dataframes with statistical details
#' that are used to create expressions displayed in `{ggstatsplot}` plots as
#' subtitle and/or as caption. Note that all of this analysis is carried out by
#' the `{statsExpressions}` package.
#'
#' For more details about underlying tests and effect size estimates, see the
#' following vignette:
#' https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html
#'
#' @return
#'
#' A list of tibbles containing statistical analysis summaries.
#'
#' @param p A plot from `{ggstatsplot}` package
#' @param ... Ignored
#'
#' @examples
#' \donttest{
#' if (require("PMCMRplus")) {
#'   set.seed(123)
#'   library(ggstatsplot)
#'
#'   # in case of group comparisons
#'   p <- ggbetweenstats(mtcars, cyl, mpg)
#'   extract_stats(p)
#'
#'   # the exact details depend on the function
#'   extract_stats(ggbarstats(mtcars, cyl, am))
#' }
#' }
#' @export

extract_stats <- function(p, ...) {
  # check the input
  if (!inherits(p, "gg")) stop("Input must be a 'ggplot' object.", call. = FALSE)

  # the exact details will depend on the function
  list(
    subtitle_data             = tryCatch(p$plot_env$subtitle_df, error = function(e) NULL),
    caption_data              = tryCatch(p$plot_env$caption_df, error = function(e) NULL),
    pairwise_comparisons_data = tryCatch(p$plot_env$mpc_df, error = function(e) NULL),
    descriptive_data          = tryCatch(p$plot_env$descriptive_df, error = function(e) NULL),
    one_sample_data           = tryCatch(p$plot_env$onesample_df, error = function(e) NULL)
  )
}

#' @noRd

eval_f <- function(.f, ...) {
  tryCatch(
    suppressWarnings(suppressMessages(exec(.f, ...))),
    error = function(e) NULL
  )
}
