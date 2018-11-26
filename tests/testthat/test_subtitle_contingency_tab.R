context("subtitle_contingency_tab")

testthat::test_that(
  desc = "subtitle_contingency_tab works",
  code = {

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      suppressWarnings(
        ggstatsplot::subtitle_contingency_tab(
          data = Titanic_full,
          main = Survived,
          condition = Class,
          stat.title = "Testing",
          k = 5,
          conf.level = .99,
          conf.type = "basic",
          nboot = 5,
          messages = FALSE
        )
      )

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          "Testing",
          italic(chi)^2,
          "(",
          3L,
          ") = ",
          "190.40110",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic(V),
          " = ",
          "0.29412",
          ", CI"["99%"],
          " [",
          "0.26560",
          ", ",
          "0.31106",
          "]",
          ", ",
          italic("n"),
          " = ",
          2201
        )
      )

    # testing overall call
    testthat::expect_identical(
      object = using_function1,
      expected = results1
    )

    # testing overall, omega squared and bayes factor
    testthat::expect_identical(
      object = as.character(using_function1)[6],
      expected = as.character(results1)[6]
    )

    # testing chi-squared value
    testthat::expect_identical(
      object = as.character(using_function1)[7],
      expected = as.character(results1)[7]
    )

    # testing Cramer's V
    testthat::expect_identical(
      object = as.character(using_function1)[15],
      expected = as.character(results1)[15]
    )

    # testing sample size
    testthat::expect_identical(
      object = using_function1[24],
      expected = results1[24]
    )
  }
)
