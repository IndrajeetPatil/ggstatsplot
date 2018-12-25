context("subtitle_anova_robust")

# conf.type = "norm" -------------------------------------------------------

testthat::test_that(
  desc = "subtitle_anova_robust works - conf.type = norm",
  code = {
    set.seed(123)

    # ggstatsplot output
    using_function1 <-
      ggstatsplot::subtitle_anova_robust(
        data = dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.5),
        x = genre,
        y = length,
        k = 5,
        tr = 0.00025,
        nboot = 2,
        messages = FALSE
      )

    # expected output
    results1 <-
      ggplot2::expr(
        paste(
          NULL,
          italic("F"),
          "(",
          "8",
          ",",
          "171.38458",
          ") = ",
          "25.18580",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic(xi),
          " = ",
          "0.60098",
          ", CI"["95%"],
          " [",
          "0.56868",
          ", ",
          "0.60232",
          "]",
          ", ",
          italic("n"),
          " = ",
          790L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)

# conf.type = "perc" -------------------------------------------------------

testthat::test_that(
  desc = "subtitle_anova_robust works - conf.type = perc",
  code = {

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      suppressWarnings(ggstatsplot::subtitle_anova_robust(
        data = dplyr::filter(ggplot2::msleep, vore != "insecti"),
        x = vore,
        y = sleep_total,
        k = 4,
        nboot = 15,
        conf.level = 0.99,
        conf.type = "basic",
        messages = TRUE
      ))

    # expected output
    results1 <-
      ggplot2::expr(
        paste(
          NULL,
          italic("F"),
          "(",
          "2",
          ",",
          "35.1708",
          ") = ",
          "0.2695",
          ", ",
          italic("p"),
          " = ",
          "0.7653",
          ", ",
          italic(xi),
          " = ",
          "0.1393",
          ", CI"["99%"],
          " [",
          "-0.1692",
          ", ",
          "0.2128",
          "]",
          ", ",
          italic("n"),
          " = ",
          71L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)
