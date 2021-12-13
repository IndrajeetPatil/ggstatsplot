# checking plot and parametric stats --------------------------------------

test_that(
  desc = "checking gghistostats plot and parametric stats - data with NAs",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <- gghistostats(
      data = dplyr::starwars,
      x = height,
      xlab = "character height",
      title = "starwars: character heights",
      binwidth = 20,
      bin.args = list(
        col = "black",
        fill = "orange",
        alpha = 0.7
      ),
      test.value = 150,
      bf.prior = 0.9
    )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data)

    expect_null(pb$layout$panel_params[[1]]$y.sec.labels, NULL)

    # checking subtitle
    set.seed(123)
    p_subtitle <- statsExpressions::one_sample_test(
      data = dplyr::starwars,
      x = height,
      type = "p",
      test.value = 150
    )$expression[[1]]

    # checking caption
    set.seed(123)
    p_cap <- statsExpressions::one_sample_test(
      data = dplyr::starwars,
      x = height,
      type = "bayes",
      test.value = 150,
      bf.prior = 0.9
    )$expression[[1]]

    # testing overall call
    expect_equal(pb$plot$labels$subtitle, p_subtitle, ignore_attr = TRUE)
    expect_equal(pb$plot$labels$caption, p_cap, ignore_attr = TRUE)
    expect_snapshot(within(pb$plot$labels, rm(subtitle, caption)))
  }
)

# checking plot and non-parametric stats -----------------------------------

test_that(
  desc = "checking gghistostats and non-parametric stats - data without NAs",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <- gghistostats(
      data = ggplot2::mpg,
      x = cty,
      xlab = "city miles per gallon",
      title = "fuel economy",
      caption = "source: government website",
      binwidth = 5,
      test.value = 20,
      k = 3,
      type = "np",
      results.subtitle = FALSE
    )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data)

    # checking panel parameters
    expect_equal(pb$layout$panel_params[[1]]$x$limits, c(7.5, 37.5))
    expect_equal(
      pb$layout$panel_params[[1]]$x$breaks,
      c(NA, 10, 20, 30, NA)
    )
    expect_equal(
      pb$layout$panel_params[[1]]$y$breaks,
      c(0, 25, 50, 75, 100)
    )
    expect_snapshot(pb$layout$panel_params[[1]]$y.sec$break_info)

    # testing labels
    expect_snapshot(pb$plot$labels)
  }
)

# checking robust stats and proportions -----------------------------------

test_that(
  desc = "checking robust stats and proportions",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <- gghistostats(
      data = mtcars,
      x = wt,
      binwidth = 0.5,
      test.value = 2.5,
      type = "r"
    ) +
      scale_x_continuous(limits = c(1, 6))

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # checking subtitle
    set.seed(123)
    p_subtitle <- statsExpressions::one_sample_test(
      data = mtcars,
      x = wt,
      test.value = 2.5,
      type = "r"
    )$expression[[1]]

    # testing labels
    expect_equal(pb$plot$labels$subtitle, p_subtitle, ignore_attr = TRUE)

    # check data
    set.seed(123)
    expect_snapshot(pb$data)
    expect_snapshot(within(pb$plot$labels, rm(subtitle)))
  }
)

# checking if normal curve works -------------------------------------

test_that(
  desc = "checking if normal curve work",
  code = {
    skip_on_cran()

    # plot
    set.seed(123)
    p1 <- gghistostats(
      data = ggplot2::msleep,
      x = awake,
      binwidth = 1,
      results.subtitle = FALSE,
      normal.curve = TRUE,
      normal.curve.args =
        list(
          color = "red",
          size = 0.8
        )
    )

    # build plots
    pb1 <- ggplot2::ggplot_build(p1)

    # check data
    set.seed(123)
    expect_snapshot(pb1$data)
    expect_snapshot(pb1$plot$labels)
  }
)

# subtitle output --------------------------------------------------

test_that(
  desc = "subtitle output",
  code = {
    skip_on_cran()

    # should output a list of length 3
    set.seed(123)
    p_sub <- gghistostats(
      data = ggplot2::msleep,
      x = brainwt,
      type = "np",
      output = "subtitle",
      test.value = 0.25
    )

    set.seed(123)
    sub <-
      statsExpressions::one_sample_test(
        data = ggplot2::msleep,
        x = brainwt,
        type = "np",
        test.value = 0.25
      )$expression[[1]]

    # tests
    expect_equal(p_sub, sub, ignore_attr = TRUE)
  }
)
