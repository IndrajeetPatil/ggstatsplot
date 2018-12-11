context("subtitle_anova_robust")

testthat::test_that(
  desc = "subtitle_anova_robust works",
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
          italic("F"),
          "(",
          8,
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
    testthat::expect_identical(
      object = using_function1,
      expected = results1
    )

    # testing denominator degrees of freedom
    testthat::expect_identical(
      object = as.character(using_function1)[8],
      expected = as.character(results1)[8]
    )

    # testing upper limit for CI
    testthat::expect_identical(
      object = as.character(using_function1)[18],
      expected = as.character(results1)[18]
    )

    # testing sample size
    testthat::expect_identical(
      object = using_function1[25],
      expected = results1[25]
    )
  }
)
