# checking default outputs -----------------------------------------

test_that("checking default outputs", {
  expect_default_contingency_plots(ggpiestats)
})

# changing labels and aesthetics -------------------------------------------

test_that("changing labels and aesthetics", {
  expect_label_plots(ggpiestats, data = mtcars, x = cyl)
  expect_custom_aesthetics_plot(ggpiestats)

  set.seed(123)
  expect_doppelganger(
    title = "label repelling works",
    fig = ggpiestats(
      mtcars,
      am,
      vs,
      label.repel = TRUE,
      results.subtitle = FALSE
    )
  )
})

# edge cases ---------------------------------------------------------

test_that("edge cases", {
  expect_dropped_levels_plots(ggpiestats)

  smokers <- tibble::tibble(
    smoker = factor(rep("no", 5L), levels = c("yes", "no"))
  )

  set.seed(123)
  expect_doppelganger(
    title = "empty groups in factors not dropped",
    fig = ggpiestats(smokers, smoker)
  )
})

# expression output --------------------------------------------------

test_that("expression output", {
  expect_contingency_subtitle(ggpiestats)
})

# pairwise comparisons --------------------------------------------------

test_that("pairwise comparisons data is returned for 3+ groups", {
  expect_pairwise_comparisons_data(ggpiestats)
})

# grouped_ggpiestats works as expected ---------------------

test_that("grouped_ggpiestats produces error when grouping variable not provided", {
  expect_snapshot_error(grouped_ggpiestats(mtcars, x = cyl))
})

test_that("grouped_ggpiestats works", {
  set.seed(123)
  expect_doppelganger(
    title = "grouped_ggpiestats with one-way table",
    fig = grouped_ggpiestats(
      mtcars,
      grouping.var = am,
      x = cyl
    )
  )

  # arm64 macOS and x86_64 Linux produce sub-pixel SVG coordinate differences
  # for this specific plot; use platform-specific snapshot variants.
  set.seed(123)
  expect_doppelganger(
    title = "grouped_ggpiestats with two-way table",
    variant = tolower(Sys.info()[["sysname"]]),
    fig = grouped_ggpiestats(
      mpg_short,
      x = cyl,
      y = class,
      grouping.var = drv,
      label.repel = TRUE
    )
  )
})

# grouped_ggpiestats edge cases --------------------

test_that("edge case behavior", {
  expect_grouped_common_legend_plot(grouped_ggpiestats)
})
