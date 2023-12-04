# entire dataset ------------------------------------------------

test_that(
  "checking ggscatterstats with entire dataset",
  {
    set.seed(123)
    expect_doppelganger(
      title = "parametric correlation - without NAs",
      fig = ggscatterstats(mtcars, wt, mpg, type = "p")
    )

    set.seed(123)
    expect_doppelganger(
      title = "robust correlation - with NAs",
      fig = ggscatterstats(ggplot2::msleep, sleep_total, brainwt, type = "r")
    )
  }
)

# aesthetic modifications work ---------------------------------------------

test_that(
  "aesthetic modifications work",
  {
    set.seed(123)
    expect_doppelganger(
      title = "changing scales and aesthetics",
      fig = ggscatterstats(
        mtcars,
        wt,
        mpg,
        results.subtitle = FALSE,
        xsidehistogram.args = list(
          fill = "red",
          color = "blue",
          na.rm = TRUE
        ),
        ysidehistogram.args = list(
          fill = "yellow",
          color = "blue",
          na.rm = TRUE
        )
      ) +
        scale_x_continuous(breaks = seq(1, 6, 1), limits = (c(1, 6))) +
        scale_y_continuous(breaks = seq(10, 40, 10), limits = (c(10, 40)))
    )
  }
)

test_that("labeling variables and expressions work as expected", {
  df <- dplyr::filter(ggplot2::msleep, conservation == "lc")

  set.seed(123)
  expect_doppelganger(
    title = "label variable and expression",
    fig = ggscatterstats(
      data = df,
      x = sleep_total,
      y = sleep_cycle,
      label.expression = sleep_total > 17,
      label.var = order,
      results.subtitle = FALSE
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "no label variable but expression",
    fig = ggscatterstats(
      data = df,
      x = sleep_total,
      y = sleep_cycle,
      label.expression = sleep_total > 17,
      label.var = NULL,
      results.subtitle = FALSE
    )
  )

  # TODO: generate snapshot on Windows machine
  # set.seed(123)
  # expect_doppelganger(
  #   title = "label variable but no expression",
  #   fig = ggscatterstats(
  #     data = df,
  #     x = sleep_total,
  #     y = sleep_cycle,
  #     label.expression = NULL,
  #     label.var = order,
  #     results.subtitle = FALSE
  #   )
  # )
})

# subtitle output ----------------------------------------------------------

test_that(
  "subtitle output - ggscatterstats",
  {
    set.seed(123)
    p_sub <- ggscatterstats(
      data = ggplot2::msleep,
      x = sleep_total,
      y = bodywt,
      conf.level = 0.90,
      type = "r"
    ) %>%
      extract_subtitle()

    set.seed(123)
    fun_sub <- corr_test(
      data = ggplot2::msleep,
      x = sleep_total,
      y = bodywt,
      conf.level = 0.90,
      type = "r"
    )$expression[[1L]]

    expect_identical(p_sub, fun_sub)
  }
)

test_that(
  "grouped_ggscatterstats plotting works as expected",
  {
    set.seed(123)
    expect_doppelganger(
      title = "defaults work as expected",
      fig = grouped_ggscatterstats(
        data = iris,
        Sepal.Length,
        Petal.Width,
        grouping.var = Species
      )
    )

    set.seed(123)
    expect_doppelganger(
      title = "aesthetic modifications work",
      fig = grouped_ggscatterstats(
        data = ggplot2::msleep,
        x = sleep_total,
        y = bodywt,
        results.subtitle = FALSE,
        grouping.var = vore,
        xlab = "total sleep",
        ylab = "body weight",
        ggplot.component = scale_y_continuous(breaks = seq(0, 6000, 1000))
      )
    )
  }
)

test_that(
  "grouped_ggscatterstats errors when no grouping is present",
  {
    expect_snapshot_error(
      grouped_ggscatterstats(
        data = iris,
        x = Sepal.Length,
        y = Petal.Width
      )
    )
  }
)
