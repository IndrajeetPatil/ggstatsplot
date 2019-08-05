context("subtitle_templates")

# zero parameter -----------------------------------------------------------

testthat::test_that(
  desc = "checking if subtitle template works without any parameter",
  code = {
    testthat::skip_on_cran()

    # subtitle
    set.seed(123)
    subtitle <- ggplot2::expr(
      paste(
        NULL,
        italic("W"),
        " = ",
        "8.74",
        ", ",
        italic("p"),
        " = ",
        "0.013",
        ", ",
        italic("r"),
        " = ",
        "0.52",
        ", CI"["99%"],
        " [",
        "0.13",
        ", ",
        "0.93",
        "]",
        ", ",
        italic("n"),
        " = ",
        32
      )
    )

    # created using a template maker
    template_0 <- ggstatsplot::subtitle_template(
      no.parameters = 0L,
      statistic.text = quote(italic("W")),
      statistic = 8.74,
      parameter = NULL,
      p.value = 0.013,
      effsize.estimate = 0.52,
      effsize.LL = 0.13,
      effsize.UL = 0.93,
      conf.level = 0.99,
      k = 2L,
      k.parameter = 0L,
      n = 32,
      effsize.text = quote(italic("r"))
    )

    # check if they are equivalent
    testthat::expect_identical(subtitle, template_0)
  }
)

# single parameter -----------------------------------------------------------

testthat::test_that(
  desc = "checking if subtitle template works with a single parameter",
  code = {
    testthat::skip_on_cran()

    # subtitle
    set.seed(123)
    subtitle <- ggplot2::expr(
      paste(
        NULL,
        italic(chi)^2,
        "(",
        "2",
        ") = ",
        "8.74",
        ", ",
        italic("p"),
        " = ",
        "0.013",
        ", ",
        italic(V),
        " = ",
        "0.52",
        ", CI"["99%"],
        " [",
        "0.13",
        ", ",
        "0.93",
        "]",
        ", ",
        italic("n"),
        " = ",
        32
      )
    )

    # created using a template maker
    template_1 <- ggstatsplot::subtitle_template(
      no.parameters = 1L,
      statistic.text = quote(italic(chi)^2),
      statistic = 8.74,
      parameter = 2,
      p.value = 0.013,
      effsize.estimate = 0.52,
      effsize.LL = 0.13,
      effsize.UL = 0.93,
      conf.level = 0.99,
      k = 2L,
      k.parameter = 0L,
      n = 32,
      effsize.text = quote(italic(V))
    )

    # check if they are equivalent
    testthat::expect_identical(subtitle, template_1)

    # make sure this doesn't work when df is not specified
    testthat::expect_error(ggstatsplot::subtitle_template(
      no.parameters = 1L,
      statistic.text = quote(italic("t")),
      statistic = 2.27,
      parameter = NULL,
      p.value = 0.107,
      effsize.estimate = 0.00,
      effsize.LL = -0.08,
      effsize.UL = 0.10,
      conf.level = 0.95,
      k = 2L,
      k.parameter = 2L,
      n = 51L,
      effsize.text = quote(omega["p"]^2)
    ))
  }
)

# two parameters -----------------------------------------------------------

testthat::test_that(
  desc = "checking if subtitle template works with two parameters",
  code = {
    testthat::skip_on_cran()

    # subtitle
    set.seed(123)
    subtitle <- ggplot2::expr(
      paste(
        NULL,
        italic("F"),
        "(",
        "3",
        ",",
        "24.05",
        ") = ",
        "2.27",
        ", ",
        italic("p"),
        " = ",
        "0.107",
        ", ",
        omega["p"]^2,
        " = ",
        "0.00",
        ", CI"["95%"],
        " [",
        "-0.08",
        ", ",
        "0.10",
        "]",
        ", ",
        italic("n"),
        " = ",
        51L
      )
    )

    # created using a template maker
    template_1 <- ggstatsplot::subtitle_template(
      no.parameters = 2L,
      statistic.text = quote(italic("F")),
      statistic = 2.27,
      parameter = 3L,
      parameter2 = 24.05,
      p.value = 0.107,
      effsize.estimate = 0.00,
      effsize.LL = -0.08,
      effsize.UL = 0.10,
      conf.level = 0.95,
      k = 2L,
      k.parameter = 0L,
      k.parameter2 = 2L,
      n = 51L,
      effsize.text = quote(omega["p"]^2)
    )

    # check if they are equivalent
    testthat::expect_identical(subtitle, template_1)

    # make sure this doesn't work when df2 is not specified
    testthat::expect_error(ggstatsplot::subtitle_template(
      no.parameters = 2L,
      statistic.text = quote(italic("F")),
      statistic = 2.27,
      parameter = 3L,
      p.value = 0.107,
      effsize.estimate = 0.00,
      effsize.LL = -0.08,
      effsize.UL = 0.10,
      conf.level = 0.95,
      k = 2L,
      k.parameter = 2L,
      n = 51L,
      effsize.text = quote(omega["p"]^2)
    ))
  }
)
