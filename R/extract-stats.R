#' @title Extracting data frames or expressions from `{ggstatsplot}` plots
#'
#' @details
#'
#' These are convenience functions to extract data frames or expressions with
#' statistical details that are used to create expressions displayed in
#' `{ggstatsplot}` plots as subtitle, caption, etc. Note that all of this
#' analysis is carried out by the `{statsExpressions}`
#' [package](https://indrajeetpatil.github.io/statsExpressions/). And so if you
#' are using these functions only to extract data frames, you are better off
#' using that package.
#'
#' The only exception is the `ggcorrmat()` function. But, if a data frame is
#' what you want, you shouldn't be using `ggcorrmat()` anyway. You can use
#' `correlation::correlation()` function which provides tidy data frames by
#' default.
#'
#' @returns
#'
#' A list of tibbles containing summaries of various statistical analyses.
#' The exact details included will depend on the function.
#'
#' @param p A plot from `{ggstatsplot}` package
#' @param ... Ignored
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
#' extract_stats(p2[[1L]])
#' extract_stats(p2[[2L]])
#' @export
extract_stats <- function(p, ...) {
  # styler: off
  list(
    subtitle_data             = tryCatch(p$plot_env$subtitle_df,    error = function(e) NULL),
    caption_data              = tryCatch(p$plot_env$caption_df,     error = function(e) NULL),
    pairwise_comparisons_data = tryCatch(p$plot_env$mpc_df,         error = function(e) NULL),
    descriptive_data          = tryCatch(p$plot_env$descriptive_df, error = function(e) NULL),
    one_sample_data           = tryCatch(p$plot_env$onesample_df,   error = function(e) NULL),
    tidy_data                 = tryCatch(p$plot_env$tidy_df,        error = function(e) NULL),
    glance_data               = tryCatch(p$plot_env$glance_df,      error = function(e) NULL)
  )
  # styler: on
}

#' @rdname extract_stats
#' @export
extract_subtitle <- function(p) purrr::pluck(extract_stats(p), "subtitle_data", "expression", 1L)

#' @rdname extract_stats
#' @export
extract_caption <- function(p) purrr::pluck(extract_stats(p), "caption_data", "expression", 1L)
