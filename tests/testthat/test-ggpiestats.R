# checking default outputs -----------------------------------------

test_that("checking default outputs", {
  set.seed(123)
  expect_doppelganger(
    title = "checking one-way table - without NA",
    fig = ggpiestats(mtcars, cyl, ratio = c(0.2, 0.2, 0.6))
  )

  set.seed(123)
  expect_doppelganger(
    title = "checking one-way table - with NA",
    fig = ggpiestats(ggplot2::msleep, vore)
  )

  set.seed(123)
  expect_doppelganger(
    title = "checking unpaired two-way table - without NA",
    fig = ggpiestats(mtcars, am, vs, ratio = c(0.4, 0.6))
  )

  set.seed(123)
  expect_doppelganger(
    title = "checking unpaired two-way table - with NA",
    fig = ggpiestats(ggplot2::msleep, conservation, vore)
  )

  set.seed(123)
  expect_doppelganger(
    title = "checking paired two-way table - without NA",
    fig = ggpiestats(
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
    fig = ggpiestats(
      survey_data_NA,
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
    fig = ggpiestats(
      mtcars,
      x = cyl,
      label = "percentage",
      results.subtitle = FALSE
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "checking count labels",
    fig = ggpiestats(
      mtcars,
      x = cyl,
      label = "counts",
      results.subtitle = FALSE
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "checking percentage and count labels",
    fig = ggpiestats(
      mtcars,
      x = cyl,
      label = "both",
      results.subtitle = FALSE
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "changing aesthetics works",
    fig = suppressWarnings(
      ggpiestats(
        mtcars,
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
  # dropped level dataset
  mtcars_small <- dplyr::filter(mtcars, am == "0")

  set.seed(123)
  expect_doppelganger(
    title = "works with dropped levels",
    fig = ggpiestats(mtcars_small, cyl, am)
  )

  set.seed(123)
  expect_doppelganger(
    title = "prop test fails with dropped levels",
    fig = ggpiestats(mtcars_small, am, cyl)
  )

  smokers <- tibble::tibble(
    smoker = factor(c("no", "no", "no", "no", "no"), levels = c("yes", "no"))
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
  set.seed(123)
  expect_doppelganger(
    title = "common legend when levels are dropped",
    fig = grouped_ggpiestats(
      dropped_levels_counts,
      x = measurement,
      y = flag,
      grouping.var = dataset,
      counts = count,
      results.subtitle = FALSE,
      proportion.test = FALSE
    )
  )
})
