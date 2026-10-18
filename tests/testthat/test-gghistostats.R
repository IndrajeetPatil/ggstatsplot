# checking default outputs -----------------------------------------

test_that("checking default outputs", {
  set.seed(123)
  expect_doppelganger(
    title = "parametric - without NA",
    fig = gghistostats(ggplot2::mpg, cty, test.value = 20, type = "p")
  )

  set.seed(123)
  expect_doppelganger(
    title = "robust - with NA",
    fig = gghistostats(
      ggplot2::msleep,
      sleep_total,
      test.value = 10,
      type = "r"
    )
  )
})

# aesthetic modifications work --------------------------------------

test_that("aesthetic modifications work", {
  set.seed(123)
  expect_doppelganger(
    title = "aesthetic modifications work",
    fig = gghistostats(
      data = mtcars,
      x = wt,
      xlab = "weight",
      title = "mtcars: car wights",
      binwidth = 5,
      bin.args = list(
        col = "black",
        fill = "orange",
        alpha = 0.7
      ),
      results.subtitle = FALSE
    )
  )
})

# custom binwidth works -------------------------------------

test_that("custom binwidth works", {
  set.seed(123)
  expect_doppelganger(
    title = "plot normal curve",
    fig = gghistostats(
      data = ggplot2::msleep,
      x = awake,
      binwidth = 1,
      results.subtitle = FALSE
    )
  )
})

# subtitle output --------------------------------------------------

test_that("subtitle output", {
  set.seed(123)
  p_sub <- gghistostats(
    data = ggplot2::msleep,
    x = brainwt,
    type = "np",
    test.value = 0.25
  ) |>
    extract_subtitle()

  set.seed(123)
  sub <- one_sample_test(
    data = ggplot2::msleep,
    x = brainwt,
    type = "np",
    test.value = 0.25
  )$expression[[1L]]

  expect_identical(p_sub, sub)
})

# grouped_gghistostats works ---------------------------------------------

test_that("grouped_gghistostats plotting works as expected", {
  set.seed(123)
  expect_doppelganger(
    title = "defaults as expected",
    fig = grouped_gghistostats(
      data = ggplot2::msleep,
      x = brainwt,
      grouping.var = vore
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "modification with ggplot2 works",
    fig = suppressMessages(grouped_gghistostats(
      data = ggplot2::msleep,
      x = brainwt,
      grouping.var = vore,
      results.subtitle = FALSE,
      ggplot.component = ggplot2::scale_x_continuous(
        sec.axis = ggplot2::dup_axis()
      )
    ))
  )
})

test_that("default binwidth ignores missing values", {
  expect_identical(.binwidth(c(1, 5, NA)), .binwidth(c(1, 5)))

  # common binwidth for grouped plots is based on non-missing values only
  p <- grouped_gghistostats(
    data = ggplot2::msleep,
    x = brainwt,
    grouping.var = vore,
    results.subtitle = FALSE
  )
  expect_identical(
    unique(purrr::map_dbl(as.list(p), \(x) {
      x$layers[[1L]]$stat_params$binwidth
    })),
    .binwidth(stats::na.omit(ggplot2::msleep$brainwt))
  )
})

test_that("histogram bins are drawn when all values are identical", {
  expect_identical(.binwidth(c(5, 5, NA)), 0.1)

  bins <- ggplot2::layer_data(
    gghistostats(data.frame(x = rep(5, 10L)), x, results.subtitle = FALSE)
  )
  expect_identical(sum(bins$count), 10)

  bins <- ggplot2::layer_data(
    gghistostats(data.frame(x = 3), x, results.subtitle = FALSE)
  )
  expect_identical(sum(bins$count), 1)
})
