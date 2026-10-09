# checking default outputs -----------------------------------------

test_that("checking default outputs", {
  set.seed(123)
  expect_doppelganger(
    title = "checking one-way table - without NA",
    fig = ggbarstats(mtcars, cyl, ratio = c(0.2, 0.2, 0.6))
  )

  set.seed(123)
  expect_doppelganger(
    title = "checking one-way table - with NA",
    fig = ggbarstats(ggplot2::msleep, vore)
  )

  set.seed(123)
  expect_doppelganger(
    title = "checking unpaired two-way table - without NA",
    fig = ggbarstats(mtcars, am, vs, ratio = c(0.4, 0.6))
  )

  set.seed(123)
  expect_doppelganger(
    title = "checking unpaired two-way table - with NA",
    fig = ggbarstats(ggplot2::msleep, conservation, vore)
  )

  set.seed(123)
  expect_doppelganger(
    title = "checking paired two-way table - without NA",
    fig = ggbarstats(
      survey_data,
      `1st survey`,
      `2nd survey`,
      counts = Counts,
      paired = TRUE,
      ratio = c(0.4, 0.6)
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "checking paired two-way table - with NA",
    fig = ggbarstats(
      data = survey_data_NA,
      x = `1st survey`,
      y = `2nd survey`,
      counts = Counts,
      paired = TRUE
    )
  )
})

# changing labels and aesthetics -------------------------------------------

test_that("changing labels and aesthetics", {
  set.seed(123)
  expect_doppelganger(
    title = "checking percentage labels",
    fig = ggbarstats(
      data = mtcars,
      x = cyl,
      y = am,
      label = "percentage",
      results.subtitle = FALSE
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "checking count labels",
    fig = ggbarstats(
      data = mtcars,
      x = cyl,
      y = am,
      label = "counts",
      results.subtitle = FALSE
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "checking percentage and count labels",
    fig = ggbarstats(
      data = mtcars,
      x = cyl,
      y = am,
      label = "both",
      results.subtitle = FALSE
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "changing aesthetics works",
    fig = suppressWarnings(
      ggbarstats(
        data = mtcars,
        x = am,
        y = cyl,
        digits.perc = 2L,
        title = "mtcars dataset",
        palette = "wesanderson::Royal2",
        ggtheme = ggplot2::theme_bw(),
        label = "counts",
        legend.title = "transmission",
        results.subtitle = FALSE
      )
    )
  )

  epoch_levels <- c("Before", "After")
  mode_levels <- c("A", "P", "C", "T")
  df <- tibble::tibble(
    epoch = factor(rep(epoch_levels, times = 4L), levels = epoch_levels),
    mode = factor(rep(mode_levels, each = 2L), levels = mode_levels),
    counts = c(30916L, 21117L, 7676L, 1962L, 1663L, 462L, 7221L, 197L)
  )

  set.seed(123)
  expect_doppelganger(
    title = "label repelling works",
    fig = ggbarstats(
      df,
      mode,
      epoch,
      counts = counts,
      label.repel = TRUE,
      type = "bayes"
    )
  )
})

# edge cases ---------------------------------------------------------

test_that("edge cases", {
  # dropped level dataset
  mtcars_small <- dplyr::filter(mtcars, am == "0")

  set.seed(123)
  expect_doppelganger(
    title = "works with dropped levels",
    fig = ggbarstats(mtcars_small, cyl, am)
  )

  set.seed(123)
  expect_doppelganger(
    title = "prop test fails with dropped levels",
    fig = ggbarstats(mtcars_small, am, cyl)
  )

  too_many_levels <- tibble::tibble(x = factor(seq_len(25L)))
  expect_error(
    ggbarstats(too_many_levels, x, results.subtitle = FALSE),
    regexp = "between 1 and 24"
  )
})

# expression output --------------------------------------------------

test_that("expression output", {
  set.seed(123)
  p_sub <- ggbarstats(
    data = ggplot2::msleep,
    x = conservation,
    y = vore,
    digits = 4L
  ) |>
    extract_subtitle()

  set.seed(123)
  stats_output <- suppressWarnings(contingency_table(
    data = ggplot2::msleep,
    x = conservation,
    y = vore,
    digits = 4L
  ))$expression[[1L]]

  expect_identical(p_sub, stats_output)
})

test_that("one-sample expression output", {
  set.seed(123)
  p_sub <- ggbarstats(mtcars, x = cyl) |> extract_subtitle()

  set.seed(123)
  stats_output <- contingency_table(
    data = mtcars,
    x = cyl
  )$expression[[1L]]

  expect_identical(p_sub, stats_output)
})

# pairwise comparisons --------------------------------------------------

test_that("pairwise comparisons data is returned for 3+ groups", {
  pairwise_data <- function(...) {
    set.seed(123)
    extract_stats(ggbarstats(...))$pairwise_comparisons_data
  }

  holm_df <- pairwise_data(mtcars, cyl, am)
  expect_s3_class(holm_df, "tbl_df")
  expect_shape(holm_df, nrow = 3L)
  expect_contains(names(holm_df), c("group1", "group2", "p.value"))

  # different p.adjust.method produces different adjusted p-values
  bonf_df <- pairwise_data(mtcars, cyl, am, p.adjust.method = "bonferroni")
  expect_s3_class(bonf_df, "tbl_df")
  expect_shape(bonf_df, nrow = 3L)
  expect_false(identical(bonf_df$p.value.adj, holm_df$p.value.adj))

  # no pairwise data for 2 levels, one-way tests, or paired tests
  expect_null(pairwise_data(mtcars, am, vs))
  expect_null(pairwise_data(mtcars, cyl))
  expect_null(pairwise_data(
    survey_data,
    `1st survey`,
    `2nd survey`,
    counts = Counts,
    paired = TRUE
  ))
})

test_that("grouped_ggbarstats produces error when grouping variable not provided", {
  expect_snapshot(grouped_ggbarstats(mtcars, x = cyl, y = am), error = TRUE)
})

test_that("grouped_ggbarstats works", {
  set.seed(123)
  expect_doppelganger(
    title = "grouped_ggbarstats with one-way table",
    fig = grouped_ggbarstats(
      mtcars,
      grouping.var = am,
      x = cyl
    )
  )

  # creating a smaller data frame
  mpg_short <- ggplot2::mpg |>
    dplyr::filter(
      drv %in% c("4", "f"),
      class %in% c("suv", "midsize"),
      trans %in% c("auto(l4)", "auto(l5)")
    )

  # when arguments are entered as bare expressions
  set.seed(123)
  expect_doppelganger(
    title = "grouped_ggbarstats with two-way table",
    fig = grouped_ggbarstats(
      data = mpg_short,
      x = cyl,
      y = class,
      grouping.var = drv,
      label.repel = TRUE
    )
  )
})

# edge cases --------------------

test_that("edge case behavior", {
  df <- data.frame(
    dataset = c("a", "b", "c", "c", "c", "c"),
    measurement = c("old", "old", "old", "old", "new", "new"),
    flag = c("no", "no", "yes", "no", "yes", "no"),
    count = c(6, 8, 8, 62, 6, 33)
  )

  set.seed(123)
  expect_doppelganger(
    title = "common legend when levels are dropped",
    fig = grouped_ggbarstats(
      data = df,
      x = measurement,
      y = flag,
      grouping.var = dataset,
      counts = count,
      results.subtitle = FALSE,
      proportion.test = FALSE
    )
  )
})
