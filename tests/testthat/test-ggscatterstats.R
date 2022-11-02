skip_if(getRversion() < "4.1")
skip_if_not_installed("ggside")

# entire dataset ------------------------------------------------

test_that(
  desc = "checking ggscatterstats with entire dataset",
  code = {
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
  desc = "aesthetic modifications work",
  code = {
    set.seed(123)
    expect_doppelganger(
      title = "changing scales and aesthetics",
      fig = ggscatterstats(mtcars, wt, mpg,
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
  desc = "subtitle output - ggscatterstats",
  code = {
    set.seed(123)
    p_sub <- ggscatterstats(
      data = dplyr::starwars,
      x = mass,
      y = height,
      conf.level = 0.90,
      type = "r",
      output = "subtitle"
    )

    set.seed(123)
    fun_sub <- corr_test(
      data = dplyr::starwars,
      x = mass,
      y = height,
      conf.level = 0.90,
      type = "r",
      output = "subtitle"
    )$expression[[1]]

    expect_equal(p_sub, fun_sub)
  }
)

test_that(
  desc = "grouped_ggscatterstats plotting works as expected",
  code = {
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
  desc = "grouped_ggscatterstats - subtitle output",
  code = {
    # data
    df <- dplyr::filter(movies_long, genre %in% c("Action Drama"))

    set.seed(123)
    ls_results <- grouped_ggscatterstats(
      data = df,
      x = rating,
      y = length,
      grouping.var = genre,
      k = 3L,
      conf.level = 0.99,
      output = "subtitle"
    )

    set.seed(123)
    basic_results <- corr_test(
      data = df,
      x = rating,
      y = length,
      k = 3L,
      conf.level = 0.99
    )$expression[[1]]


    expect_equal(length(ls_results), 1L)
    expect_equal(ls_results$`Action Drama`, basic_results, ignore_attr = TRUE)
  }
)

test_that(
  desc = "grouped_ggscatterstats errors when no grouping is present",
  code = {
    expect_snapshot_error(
      grouped_ggscatterstats(
        data = iris,
        x = Sepal.Length,
        y = Petal.Width,
      )
    )
  }
)
