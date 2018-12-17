context("subtitle_ggscatterstats")

# nonparametric ----------------------------------------------------------

testthat::test_that(
  desc = "subtitle_ggscatterstats works - nonparametric",
  code = {
    set.seed(123)
    using_function1 <-
      suppressWarnings(ggstatsplot::subtitle_ggscatterstats(
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
  }
)

# parametric ----------------------------------------------------------

testthat::test_that(
  desc = "subtitle_ggscatterstats works - parametric",
  code = {
    set.seed(123)
    using_function1 <-
      suppressWarnings(ggstatsplot::subtitle_ggscatterstats(
        data = ggplot2::msleep,
        x = brainwt,
        y = sleep_rem,
        type = "parametric",
        k = 3,
        conf.level = .90,
        conf.type = "bca",
        nboot = 25,
        messages = FALSE
      ))

    results1 <-
      ggplot2::expr(
        paste(
          italic("r")["pearson"],
          "(",
          46L,
          ")",
          " = ",
          "-0.221",
          ", CI"["90%"],
          " [",
          "-0.438",
          ", ",
          "0.020",
          "], ",
          italic("p"),
          " = ",
          "0.131",
          ", ",
          italic("n"),
          " = ",
          48L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)


# robust ----------------------------------------------------------

testthat::test_that(
  desc = "subtitle_ggscatterstats works - robust",
  code = {
    set.seed(123)
    using_function1 <-
      suppressWarnings(ggstatsplot::subtitle_ggscatterstats(
        data = ggplot2::msleep,
        x = "brainwt",
        y = "sleep_total",
        type = "r",
        k = 4,
        conf.level = .50,
        conf.type = "basic",
        nboot = 25,
        messages = FALSE
      ))

    results1 <-
      ggplot2::expr(
        paste(
          italic(rho)["pb"],
          " = ",
          "-0.5696",
          ", CI"["50%"],
          " [",
          "-0.6047",
          ", ",
          "-0.5283",
          "], ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("n"),
          " = ",
          56L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)
