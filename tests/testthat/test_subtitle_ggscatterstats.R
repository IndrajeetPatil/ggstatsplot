context("subtitle_ggscatterstats")

testthat::test_that(
  desc = "subtitle_ggscatterstats works",
  code = {
    set.seed(123)
    using_function1 <-
      suppressWarnings(ggstatsplot:::subtitle_ggscatterstats(
        data = movies_long,
        x = rating,
        y = length,
        type = "nonparametric",
        k = 5,
        conf.level = .999,
        conf.type = "perc",
        nboot = 50,
        messages = FALSE
      ))

    results1 <-
      ggplot2::expr(
        paste(
          italic(rho)["spearman"],
          "(",
          1577,
          ")",
          " = ",
          "0.46669",
          ", CI"["99.9%"],
          " [",
          "0.40415",
          ", ",
          "0.50080",
          "], ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("n"),
          " = ",
          1579L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)

    # testing
    testthat::expect_identical(
      as.character(using_function1)[8],
      as.character(results1)[8]
    )

    # testing
    testthat::expect_identical(
      as.character(using_function1)[12],
      as.character(results1)[12]
    )
  }
)
