context("subtitle_t_onesample")

# parametric -----------------------------------------------------------

testthat::test_that(
  desc = "subtitle_t_onesample parametric works",
  code = {

    # ggstatsplot output
    set.seed(123)
    using_function <-
      suppressWarnings(
        ggstatsplot::subtitle_t_onesample(
          data = movies_long,
          x = length,
          test.value = 120,
          type = "p",
          k = 5,
          messages = FALSE
        )
      )

    # expected output
    set.seed(123)
    results <-
      ggplot2::expr(
        paste(
          NULL,
          italic("t"),
          "(",
          "1578",
          ") = ",
          "-23.04633",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("d"),
          " = ",
          "-0.57998",
          ", CI"["95%"],
          " [",
          "-0.63321",
          ", ",
          "-0.52659",
          "]",
          ", ",
          italic("n"),
          " = ",
          1579L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function, results)
  }
)

# non-parametric -----------------------------------------------------------

testthat::test_that(
  desc = "subtitle_t_onesample non-parametric works",
  code = {

    # ggstatsplot output
    set.seed(123)
    using_function <-
      suppressWarnings(
        ggstatsplot::subtitle_t_onesample(
          data = ToothGrowth,
          x = len,
          test.value = 20,
          type = "np",
          k = 4,
          messages = FALSE
        )
      )

    # expected output
    set.seed(123)
    results <-
      ggplot2::expr(
        paste(
          NULL,
          "log"["e"](italic("W")),
          " = ",
          "6.6247",
          ", ",
          italic("p"),
          " = ",
          "0.3227",
          ", ",
          Delta["HLS"],
          " = ",
          "18.8499",
          ", CI"["95%"],
          " [",
          "16.6500",
          ", ",
          "21.0500",
          "]",
          ", ",
          italic("n"),
          " = ",
          60L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function, results)
  }
)


# robust -----------------------------------------------------------

testthat::test_that(
  desc = "subtitle_t_onesample robust works",
  code = {

    # ggstatsplot output
    set.seed(123)
    using_function <-
      suppressWarnings(
        ggstatsplot::subtitle_t_onesample(
          x = anscombe$x1,
          test.value = 8,
          type = "r",
          k = 4,
          messages = TRUE
        )
      )

    # expected output
    set.seed(123)
    results <-
      ggplot2::expr(
        paste(
          italic("M")[robust],
          " = ",
          "9.0000",
          ", CI"["95%"],
          " [",
          "6.8434",
          ", ",
          "11.3163",
          "], ",
          italic("p"),
          " = ",
          "0.3000",
          ", ",
          italic("n"),
          " = ",
          11L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function, results)
  }
)

# bayes factor -----------------------------------------------------------

testthat::test_that(
  desc = "subtitle_t_onesample bayes factor works",
  code = {

    # ggstatsplot output
    set.seed(123)
    using_function <-
      suppressWarnings(
        ggstatsplot::subtitle_t_onesample(
          x = anscombe$x2,
          test.value = 8,
          type = "bf",
          messages = FALSE
        )
      )

    # expected output
    set.seed(123)
    results <-
      ggplot2::expr(
        paste(
          italic("t"),
          "(",
          10,
          ") = ",
          "1.00",
          ", log"["e"],
          "(BF"["10"],
          ") = ",
          "-0.80",
          ", Prior width = ",
          "0.71",
          ", ",
          italic("d"),
          " = ",
          "0.30",
          ", ",
          italic("n"),
          " = ",
          11L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function, results)
  }
)
