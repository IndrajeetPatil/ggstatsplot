#' Get stats from grouped plots
#'
#' @param p A grouped ggstatsplot (made with any of the grouped_ functions)
#'
#' @return A tibble summarizing stats for a grouped plot made with ggstastplot
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' set.seed(123)
#'
#' # grouped plot
#' p <- grouped_ggbarstats(Titanic_full, Survived, Sex, grouping.var = Age)
#'
#' extract_grouped_stats(p)
#' @export
extract_grouped_stats <- function(p = NULL) {
  stats_0 <- NULL
  for (plot_i in seq_along(p)) {
    table_i = tryCatch(p[[plot_i]]$plot_env$subtitle_df, error = function(e) NULL)
    Group_i = tryCatch(p[[plot_i]]$labels$title, error = function(e) NULL)
    # Column 'expression' is a list, in order to make the output compatible
    # with exporting functions as openxlsx::save_xlsx it must be converted to
    # a vector of chars
    table_i$expression <- as.character(table_i$expression)
    stats_i <- data.frame(Group_i, table_i)
    stats_0 <- rbind(stats_0, stats_i)
  }
  tibble::tibble(stats_0)
}
