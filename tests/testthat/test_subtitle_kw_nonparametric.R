context("subtitle_kw_nonparametric")

testthat::test_that(
  desc = "subtitle_kw_nonparametric works",
  code = {

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      ggstatsplot::subtitle_kw_nonparametric(
        data = ggstatsplot::movies_long,
        x = genre,
        y = length,
        k = 5,
        messages = FALSE
      )

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          "Kruskal-Wallis: ",
          italic(chi)^2,
          "(",
          5L,
          ") = ",
          "283.48849",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          eta["H"]^2,
          " = ",
          "0.11475",
          ", CI"["95%"],
          " [",
          "0.09169",
          ", ",
          "0.13793",
          "]",
          ", ",
          italic("n"),
          " = ",
          2433L
        )
      )

    # testing overall call, omega squared and bayes factor
    testthat::expect_identical(
      using_function1,
      results1
    )

    # testing degress of freedom for chi-squared statistic
    testthat::expect_identical(
      as.character(using_function1)[5],
      as.character(results1)[5]
    )

    # testing chi-squared statistic
    testthat::expect_identical(
      as.character(using_function1)[7],
      as.character(results1)[7]
    )

    # testing sample size
    testthat::expect_identical(
      using_function1[24],
      results1[24]
    )
  }
)
