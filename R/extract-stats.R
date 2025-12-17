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
#' [`correlation::correlation()`] function which provides tidy data frames by
#' default.
#'
#' @returns
#'
#' A list of tibbles containing summaries of various statistical analyses.
#' The exact details included will depend on the function.
#'
#' @param p A plot from `{ggstatsplot}` package
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
extract_stats <- function(p) {
  if (inherits(p, "patchwork")) purrr::map(.extract_plots(p), .extract_stats) else .extract_stats(p)
}

.extract_plots <- function(p) purrr::map(seq_along(p), \(i) magrittr::extract2(p, i))

.pluck_plot_env <- function(p, data) purrr::pluck(p, "plot_env", data)

.extract_stats <- function(p) {
  # styler: off
  structure(list(
    subtitle_data             = .pluck_plot_env(p, "subtitle_df"),
    caption_data              = .pluck_plot_env(p, "caption_df"),
    pairwise_comparisons_data = .pluck_plot_env(p, "mpc_df"),
    descriptive_data          = .pluck_plot_env(p, "descriptive_df"),
    one_sample_data           = .pluck_plot_env(p, "onesample_df"),
    tidy_data                 = .pluck_plot_env(p, "tidy_df"),
    glance_data               = .pluck_plot_env(p, "glance_df")
  ), class = c("ggstatsplot_stats", "list"))
  # styler: on
}


# function factory to extract particular kind of stats data
.extract_stats_data <- function(data_component) {
  \(p) {
    dat <- extract_stats(p)
    .pluck_expression <- \(x) purrr::pluck(x, data_component, "expression", 1L)
    if (inherits(dat, "ggstatsplot_stats")) .pluck_expression(dat) else purrr::map(dat, .pluck_expression)
  }
}

#' @rdname extract_stats
#' @export
extract_subtitle <- .extract_stats_data("subtitle_data")

#' @rdname extract_stats
#' @export
extract_caption <- .extract_stats_data("caption_data")
