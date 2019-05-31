context("subtitle_ggscatterstats")

# nonparametric ----------------------------------------------------------

testthat::test_that(
  desc = "subtitle_ggscatterstats works - nonparametric",
  code = {
    testthat::skip_on_cran()

    # ggstatsplot output
    set.seed(123)
    using_function <-
      suppressWarnings(ggstatsplot::subtitle_ggscatterstats(
        data = movies_long,
        x = rating,
        y = length,
        type = "nonparametric",
        k = 5,
        conf.level = 0.999,
        nboot = 50,
        messages = FALSE
      ))

    # expected
    expected <-
      ggplot2::expr(
        paste(
          NULL,
          "log"["e"](italic("S")),
          " = ",
          "19.67322",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic(rho)["Spearman"],
          " = ",
          "0.46669",
          ", CI"["99.9%"],
          " [",
          "0.39941",
          ", ",
          "0.52898",
          "]",
          ", ",
          italic("n"),
          " = ",
          1579L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function, expected)
  }
)

# parametric --------------------------------------------------------------

testthat::test_that(
  desc = "subtitle_ggscatterstats works - parametric",
  code = {
    testthat::skip_on_cran()

    # ggstatsplot output
    set.seed(123)
    using_function <-
      suppressWarnings(ggstatsplot::subtitle_ggscatterstats(
        data = ggplot2::msleep,
        x = brainwt,
        y = sleep_rem,
        type = "parametric",
        k = 3,
        conf.level = 0.90,
        conf.type = "bca",
        nboot = 25,
        messages = FALSE
      ))

    # expected
    expected <-
      ggplot2::expr(
        paste(
          NULL,
          italic("t"),
          "(",
          "46",
          ") = ",
          "-1.539",
          ", ",
          italic("p"),
          " = ",
          "0.131",
          ", ",
          italic("r")["Pearson"],
          " = ",
          "-0.221",
          ", CI"["90%"],
          " [",
          "-0.438",
          ", ",
          "0.020",
          "]",
          ", ",
          italic("n"),
          " = ",
          48L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function, expected)
  }
)

# robust ----------------------------------------------------------------

testthat::test_that(
  desc = "subtitle_ggscatterstats works - robust",
  code = {
    testthat::skip_on_cran()


    # using function
    set.seed(123)
    using_function <-
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

    # expected
    expected <-
      ggplot2::expr(
        paste(
          NULL,
          italic("t"),
          "(",
          "54",
          ") = ",
          "-5.0929",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic(rho)["pb"],
          " = ",
          "-0.5696",
          ", CI"["50%"],
          " [",
          "-0.6432",
          ", ",
          "-0.4927",
          "]",
          ", ",
          italic("n"),
          " = ",
          56L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function, expected)
  }
)


# bayes ----------------------------------------------------------------

testthat::test_that(
  desc = "subtitle_ggscatterstats works - bayes",
  code = {
    testthat::skip_on_cran()

    # using function
    set.seed(123)
    using_function <-
      suppressWarnings(ggstatsplot::subtitle_ggscatterstats(
        data = ggplot2::msleep,
        x = "brainwt",
        y = sleep_rem,
        type = "bf",
        k = 3,
        messages = FALSE
      ))

    # expected
    expected <-
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "-0.425",
          ", ",
          italic("r")["Cauchy"],
          " = ",
          "0.707"
        )
      ))

    # testing overall call
    testthat::expect_identical(using_function, expected)
  }
)
