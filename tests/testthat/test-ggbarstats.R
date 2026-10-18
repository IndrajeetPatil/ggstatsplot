# checking default outputs -----------------------------------------

test_that("checking default outputs", {
  expect_default_contingency_plots(ggbarstats)
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

  # `ggbarstats()` has no `label.repel` argument; the title is kept so the
  # snapshot file name stays the same
  set.seed(123)
  expect_doppelganger(
    title = "label repelling works",
    fig = ggbarstats(df, mode, epoch, counts = counts, type = "bayes")
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

  # no proportion test labels when `x` has a single level
  p_label_layers <- function(p) {
    layer_labels <- purrr::map_chr(
      p$layers,
      \(l) rlang::as_label(l$mapping$label)
    )
    sum(layer_labels == ".p.label")
  }
  expect_identical(p_label_layers(ggbarstats(mtcars_small, am, cyl)), 0L)
  expect_identical(p_label_layers(ggbarstats(mtcars, am, cyl)), 1L)

  too_many_levels <- tibble::tibble(x = factor(seq_len(25L)))
  expect_error(
    ggbarstats(too_many_levels, x, results.subtitle = FALSE),
    regexp = "between 1 and 24"
  )
})

# expression output --------------------------------------------------

test_that("expression output", {
  expect_contingency_subtitle(ggbarstats)
})

# pairwise comparisons --------------------------------------------------

test_that("pairwise comparisons data is returned for 3+ groups", {
  expect_pairwise_comparisons_data(ggbarstats)
})

# grouped_ggbarstats works as expected ---------------------

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

  set.seed(123)
  expect_doppelganger(
    title = "grouped_ggbarstats with two-way table",
    fig = grouped_ggbarstats(
      data = mpg_short,
      x = cyl,
      y = class,
      grouping.var = drv
    )
  )
})

# grouped_ggbarstats edge cases --------------------

test_that("edge case behavior", {
  set.seed(123)
  expect_doppelganger(
    title = "common legend when levels are dropped",
    fig = grouped_ggbarstats(
      data = dropped_levels_counts,
      x = measurement,
      y = flag,
      grouping.var = dataset,
      counts = count,
      results.subtitle = FALSE,
      proportion.test = FALSE
    )
  )
})
