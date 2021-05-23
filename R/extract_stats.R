#' @title Extracting dataframes with statistical details from `ggstatsplot`
#'
#' @details
#'
#' This is a convenience function to extract dataframes with statistical details
#' that are used to create expressions displayed in `ggstatsplot` plots as
#' subtitle and/or as caption. Note that all of this analysis is carried out by
#' the `statsExpressions` package.
#'
#' For more details about underlying tests and effect size estimates, see the
#' following vignette:
#' https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html
#'
#' @note
#' Note that if you want to use this function with `ggscatterstats`, you will
#' have to set `marginal = FALSE` to return an object of `ggplot` type.
#'
#' @param p A plot from `ggstatsplot` package
#' @param ... Ignored
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' library(ggstatsplot)
#' p <- ggbetweenstats(mtcars, am, mpg)
#' extract_stats(p)
#' }
#' @export

extract_stats <- function(p, ...) {
  # check the input
  if (!inherits(p, "gg")) stop("Input must be a 'ggplot' object.", call. = FALSE)

  # caption dataframe might not be present when `type` is not "parametric"
  list(
    subtitle_data = tryCatch(p$plot_env$subtitle_df, error = function(e) NULL),
    caption_data = tryCatch(p$plot_env$caption_df, error = function(e) NULL)
  )
}
