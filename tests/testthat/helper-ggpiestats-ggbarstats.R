# shared fixtures and checks for test-ggpiestats.R and test-ggbarstats.R

# small two-way table for grouped plots
mpg_short <- dplyr::filter(
  ggplot2::mpg,
  drv %in% c("4", "f"),
  class %in% c("suv", "midsize"),
  trans %in% c("auto(l4)", "auto(l5)")
)

# pre-aggregated counts where some `dataset` groups lack `measurement` levels
dropped_levels_counts <- data.frame(
  dataset = c("a", "b", "c", "c", "c", "c"),
  measurement = c("old", "old", "old", "old", "new", "new"),
  flag = c("no", "no", "yes", "no", "yes", "no"),
  count = c(6, 8, 8, 62, 6, 33)
)

# the subtitle matches the `statsExpressions::contingency_table()` expression
expect_contingency_subtitle <- function(plot_fn) {
  set.seed(123)
  plot_subtitle <- extract_subtitle(plot_fn(
    ggplot2::msleep,
    x = conservation,
    y = vore,
    digits = 4L
  ))

  set.seed(123)
  stats_output <- suppressWarnings(statsExpressions::contingency_table(
    ggplot2::msleep,
    x = conservation,
    y = vore,
    digits = 4L
  ))

  expect_identical(plot_subtitle, stats_output$expression[[1L]])
}

# pairwise comparisons are returned only for unpaired two-way tables with 3+
# `x` levels, and respect `p.adjust.method`
expect_pairwise_comparisons_data <- function(plot_fn) {
  pairwise_data <- function(...) {
    set.seed(123)
    extract_stats(plot_fn(...))$pairwise_comparisons_data
  }

  holm_df <- pairwise_data(mtcars, cyl, am)
  expect_s3_class(holm_df, "tbl_df")
  expect_shape(holm_df, nrow = 3L)
  expect_contains(names(holm_df), c("group1", "group2", "p.value"))

  bonf_df <- pairwise_data(mtcars, cyl, am, p.adjust.method = "bonferroni")
  expect_shape(bonf_df, nrow = 3L)
  expect_false(identical(bonf_df$p.value.adj, holm_df$p.value.adj))

  expect_null(pairwise_data(mtcars, am, vs))
  expect_null(pairwise_data(mtcars, cyl))
  expect_null(pairwise_data(
    survey_data,
    `1st survey`,
    `2nd survey`,
    counts = Counts,
    paired = TRUE
  ))
}
