context("subtitle_contingency_tab_gof")

testthat::test_that(
  desc = "Goodness of Fit subtitle_contingency_tab works",
  code = {

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      suppressWarnings(
        ggstatsplot::subtitle_onesample_proptest(
          data = Titanic_full,
          main = Class,
          legend.title = "Testing",
          k = 5
        )
      )

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          italic(chi)^2,
          "(",
          4,
          ") = ",
          "1135.00863",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("n"),
          " = ",
          2201L
        )
      )

    # testing overall call
    testthat::expect_identical(
      object = using_function1,
      expected = results1
    )

    # testing chi-squared value
    testthat::expect_identical(
      object = as.character(using_function1)[6],
      expected = as.character(results1)[6]
    )

    # testing sample size
    testthat::expect_identical(
      object = using_function1[14],
      expected = results1[14]
    )
  }
)
