# checking plot and parametric stats --------------------------------------

test_that(
  desc = "checking gghistostats plot and parametric stats - data with NAs",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::gghistostats(
        data = dplyr::starwars,
        x = height,
        xlab = "character height",
        title = "starwars: character heights",
        binwidth = 20,
        bar.fill = "orange",
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
    p_subtitle <-
      statsExpressions::one_sample_test(
        data = dplyr::starwars,
        x = height,
        type = "p",
        test.value = 150
      )$expression[[1]]

    # checking caption
    set.seed(123)
    p_cap <-
      statsExpressions::one_sample_test(
        data = dplyr::starwars,
        x = height,
        type = "bayes",
        test.value = 150,
        bf.prior = 0.9
      )$expression[[1]]

    # testing overall call
    expect_identical(pb$plot$labels$subtitle, p_subtitle)
    expect_identical(pb$plot$labels$title, "starwars: character heights")
    expect_identical(pb$plot$labels$x, "character height")
    expect_identical(pb$plot$labels$caption, p_cap)
  }
)

# checking plot and non-parametric stats -----------------------------------

test_that(
  desc = "checking gghistostats and non-parametric stats - data without NAs",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::gghistostats(
        data = ggplot2::mpg,
        x = cty,
        xlab = "city miles per gallon",
        title = "fuel economy",
        caption = substitute(paste(italic("source"), ": government website")),
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
    expect_identical(
      pb$layout$panel_params[[1]]$x$breaks,
      c(NA, 10, 20, 30, NA)
    )
    expect_identical(
      pb$layout$panel_params[[1]]$y$breaks,
      c(0, 25, 50, 75, 100)
    )
    expect_equal(
      pb$layout$panel_params[[1]]$y.sec$break_info,
      list(
        range = c(-0.0211538461538462, 0.444230769230769),
        labels = c("0%", "10%", "20%", "30%", "40%"),
        major = c(
          0.045, 0.26, 0.475, 0.69,
          0.905
        ),
        minor = c(
          0.045, 0.153, 0.26, 0.367, 0.475, 0.583, 0.69,
          0.798, 0.905
        ),
        major_source = c(
          -0.044594594594594,
          23.3923423423423,
          46.8292792792793,
          70.1572072072072,
          93.5941441441442
        ),
        minor_source = c(
          -0.044594594594594,
          11.7283783783784,
          23.3923423423423,
          35.0563063063063,
          46.8292792792793,
          58.4932432432432,
          70.1572072072072,
          81.9301801801802,
          93.5941441441442
        ),
        major_source_user = c(0, 0.1, 0.2, 0.3, 0.4),
        minor_source_user = c(
          0,
          0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4
        )
      )
    )

    # testing labels
    expect_identical(p$labels$subtitle, NULL)
    expect_identical(p$labels$title, "fuel economy")
    expect_identical(p$labels$x, "city miles per gallon")
    expect_identical(p$labels$y, "count")
    expect_identical(
      p$labels$caption,
      ggplot2::expr(paste(
        italic("source"),
        ": government website"
      ))
    )
  }
)

# checking robust stats and proportions -----------------------------------

test_that(
  desc = "checking robust stats and proportions",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::gghistostats(
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
    p_subtitle <-
      statsExpressions::one_sample_test(
        data = mtcars,
        x = wt,
        test.value = 2.5,
        type = "r"
      )$expression[[1]]

    # testing labels
    expect_identical(pb$plot$labels$subtitle, p_subtitle)
    expect_null(pb$plot$labels$caption, NULL)
    expect_identical(pb$plot$labels$y, "count")

    # check data
    set.seed(123)
    expect_snapshot(pb$data)
  }
)

# checking if normal curve works -------------------------------------

test_that(
  desc = "checking if normal curve work",
  code = {
    skip_on_cran()

    # plot
    set.seed(123)
    p1 <-
      ggstatsplot::gghistostats(
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

    # annotation
    expect_equal(
      pb1$plot$labels,
      list(
        x = "awake",
        y = "count",
        title = NULL,
        subtitle = NULL,
        caption = NULL,
        fill = "count",
        weight = "weight",
        xintercept = "xintercept"
      )
    )
  }
)

# subtitle output --------------------------------------------------

test_that(
  desc = "subtitle output",
  code = {
    skip_on_cran()

    # should output a list of length 3
    set.seed(123)
    p_sub <-
      ggstatsplot::gghistostats(
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
    expect_identical(p_sub, sub)
  }
)
