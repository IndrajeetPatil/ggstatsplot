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
    testthat::expect_identical(using_function1, results1)
  }
)
