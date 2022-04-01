#' @title Extracting data frames from `{ggstatsplot}` plots
#'
#' @details
#'
#' This is a convenience function to extract data frames with statistical details
#' that are used to create expressions displayed in `{ggstatsplot}` plots as
#' subtitle, caption, etc. Note that all of this analysis is carried out by
#' the `{statsExpressions}` [package](https://indrajeetpatil.github.io/statsExpressions).
#'
#' The only exception is the `ggcorrmat()` function. But, if a data frame is
#' what you want, you shouldn't be using `ggcorrmat()` anyway. You can use
#' `correlation::correlation()` function which provides tidy data frames by
#' default. This also works if the data entered is grouped (*a la*
#' `dplyr::group_by()`). This is also the function used internally by
#' `{ggstatsplot}` to extract a data frame used to create a plot.
#'
#' @return
#'
#' A list of tibbles containing summaries of various statistical analyses.
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
  # works only with ggplot objects
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
