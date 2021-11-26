# pearson's r with NAs ---------------------------------------------

test_that(
  desc = "checking ggscatterstats - without NAs - pearson's r",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggscatterstats(
        data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
        x = sleep_total,
        y = "sleep_cycle",
        label.var = "name",
        label.expression = sleep_cycle > 0.3,
        xlab = "sleep (total)",
        ylab = "sleep cycle",
        type = "p",
        xfill = "red",
        yfill = "orange",
        marginal = FALSE,
        caption = "ggplot2 dataset",
        title = "Mammalian sleep"
      )

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(list(pb$data[[1]], head(pb$data[[2]]), pb$data[[3]]))

    # subtitle
    set.seed(123)
    p_subtitle <-
      statsExpressions::corr_test(
        data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
        x = "sleep_total",
        y = sleep_cycle,
        type = "p"
      )$expression[[1]]

    # subtitle
    set.seed(123)
    p_cap <-
      statsExpressions::corr_test(
        data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
        x = "sleep_total",
        y = sleep_cycle,
        top.text = "ggplot2 dataset",
        type = "bayes"
      )$expression[[1]]

    # checking plot labels
    expect_equal(pb$plot$labels$caption, p_cap)
    expect_equal(pb$plot$labels$subtitle, p_subtitle, ignore_attr = TRUE)
    expect_snapshot(within(pb$plot$labels, rm(subtitle, caption)))
  }
)

# spearman's rho with NAs ---------------------------------------------

test_that(
  desc = "checking ggscatterstats - without NAs - spearman's rho",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggscatterstats(
        data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
        x = "sleep_total",
        y = sleep_cycle,
        type = "np",
        conf.level = 0.99,
        marginal = FALSE
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data[[1]])

    # subtitle
    set.seed(123)
    p_subtitle <-
      statsExpressions::corr_test(
        data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
        x = sleep_total,
        y = sleep_cycle,
        type = "np",
        conf.level = 0.99
      )$expression[[1]]

    # testing data and annotations
    expect_equal(pb$plot$labels$subtitle, p_subtitle, ignore_attr = TRUE)
    expect_snapshot(within(pb$plot$labels, rm(subtitle)))
  }
)


# winsorized Pearson with NAs ---------------------------------------------

test_that(
  desc = "checking ggscatterstats - without NAs - winsorized Pearson",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <- ggscatterstats(
      data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
      x = sleep_total,
      y = sleep_cycle,
      type = "r",
      conf.level = 0.90,
      point.args = list(color = "red", size = 5, stroke = 0),
      marginal = FALSE
    )

    # subtitle
    set.seed(123)
    p_subtitle <- statsExpressions::corr_test(
      data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
      x = sleep_total,
      y = sleep_cycle,
      type = "r",
      conf.level = 0.90
    )$expression[[1]]

    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data[[1]])

    expect_equal(pb$plot$labels$subtitle, p_subtitle, ignore_attr = TRUE)
    expect_snapshot(within(pb$plot$labels, rm(subtitle)))
  }
)

# bayes factor plus class of object -----------------------------------------

test_that(
  desc = "bayes factor plus class of object",
  code = {
    skip_on_cran()
    skip_if_not_installed("ggside")

    # creating the plot
    set.seed(123)
    p <- ggscatterstats(
      data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
      x = sleep_total,
      y = sleep_cycle,
      xlab = "total sleep",
      ylab = "sleep cycle",
      title = "mammalian sleep dataset",
      caption = "source: ggplot2 package",
      type = "bayes",
      ggplot.component = ggplot2::scale_y_continuous(breaks = seq(0, 6000, 1000))
    )

    pb <- ggplot2::ggplot_build(p)

    # subtitle
    set.seed(123)
    p_subtitle <- statsExpressions::corr_test(
      data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
      x = sleep_total,
      y = sleep_cycle,
      type = "bayes",
      top.text = "source: ggplot2 package"
    )$expression[[1]]

    # check just the names and dims and not the actual values
    set.seed(123)
    expect_snapshot(purrr::map(pb$data, names))
    expect_snapshot(purrr::map(pb$data, dim))

    expect_equal(pb$plot$labels$subtitle, p_subtitle, ignore_attr = TRUE)
  }
)

# aesthetic modifications work ---------------------------------------------

test_that(
  desc = "aesthetic modifications work",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <- ggscatterstats(
      data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
      x = sleep_total,
      y = sleep_cycle,
      label.expression = sleep_total > 17,
      label.var = order,
      point.label.args = list(size = 4, color = "blue", alpha = 0.5),
      ggplot.component = list(
        ggplot2::coord_cartesian(ylim = c(0, 7000)),
        ggplot2::scale_y_continuous(breaks = seq(0, 7000, 1000))
      ),
      results.subtitle = FALSE
    )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(list(pb$data[[1]], head(pb$data[[2]]), pb$data[[3]]))
    expect_snapshot(pb$plot$labels)
    expect_s3_class(p, "gg")
  }
)

# subtitle output ----------------------------------------------------------

test_that(
  desc = "subtitle output",
  code = {
    skip_on_cran()

    # creating the messages
    set.seed(123)
    p_sub <-
      ggscatterstats(
        data = dplyr::starwars,
        x = mass,
        y = height,
        conf.level = 0.90,
        type = "r",
        output = "subtitle"
      )

    fun_sub <-
      statsExpressions::corr_test(
        data = dplyr::starwars,
        x = mass,
        y = height,
        conf.level = 0.90,
        type = "r",
        output = "subtitle"
      )$expression[[1]]

    # checking captured messages
    expect_equal(p_sub, fun_sub)
  }
)

test_that("plots are rendered correctly - ggscatterstats", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")
  skip_if(getRversion() >= "4.2")

  # vdiffr tests --------------------------------

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "label symbol args - vdiffr",
    fig = ggscatterstats(
      data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
      x = sleep_total,
      y = sleep_cycle,
      label.expression = sleep_total > 17,
      label.var = order,
      results.subtitle = FALSE
    )
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "label var NULL - vdiffr",
    fig = ggscatterstats(
      data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
      x = sleep_total,
      y = sleep_cycle,
      label.expression = sleep_total > 17,
      label.var = NULL,
      results.subtitle = FALSE
    )
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "label expr NULL - vdiffr",
    fig = ggscatterstats(
      data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
      x = sleep_total,
      y = sleep_cycle,
      label.expression = NULL,
      label.var = order,
      results.subtitle = FALSE
    )
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "changing scales and aesthetics - vdiffr",
    fig = ggscatterstats(mtcars, wt, mpg,
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
})
