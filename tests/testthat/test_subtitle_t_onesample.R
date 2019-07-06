context("subtitle_t_onesample")

# parametric -----------------------------------------------------------

testthat::test_that(
  desc = "subtitle_t_onesample parametric works",
  code = {
    testthat::skip_on_cran()

    # Hedge's g and non-central
    set.seed(123)
    using_function1 <-
      ggstatsplot::subtitle_t_onesample(
        data = movies_long,
        x = length,
        test.value = 120,
        type = "p",
        k = 5,
        messages = FALSE
      )

    set.seed(123)
    results1 <-
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
          italic("g"),
          " = ",
          "-0.57970",
          ", CI"["95%"],
          " [",
          "-0.63341",
          ", ",
          "-0.52675",
          "]",
          ", ",
          italic("n"),
          " = ",
          1579L
        )
      )

    # Hedge's g and central
    set.seed(123)
    using_function2 <-
      suppressWarnings(
        ggstatsplot::subtitle_t_onesample(
          data = movies_long,
          x = length,
          test.value = 120,
          type = "p",
          effsize.noncentral = FALSE,
          k = 3,
          conf.level = 0.99,
          messages = FALSE
        )
      )

    set.seed(123)
    results2 <-
      ggplot2::expr(
        paste(
          NULL,
          italic("t"),
          "(",
          "1578",
          ") = ",
          "-23.046",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("g"),
          " = ",
          "-0.580",
          ", CI"["99%"],
          " [",
          "-0.650",
          ", ",
          "-0.510",
          "]",
          ", ",
          italic("n"),
          " = ",
          1579L
        )
      )

    # Cohen's d and non-central
    set.seed(123)
    using_function3 <-
      suppressWarnings(
        ggstatsplot::subtitle_t_onesample(
          data = movies_long,
          x = length,
          test.value = 120,
          type = "p",
          effsize.type = "d",
          effsize.noncentral = TRUE,
          k = 4,
          conf.level = 0.90,
          conf.type = "bca",
          messages = FALSE
        )
      )

    set.seed(123)
    results3 <-
      ggplot2::expr(
        paste(
          NULL,
          italic("t"),
          "(",
          "1578",
          ") = ",
          "-23.0463",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("d"),
          " = ",
          "-0.5800",
          ", CI"["90%"],
          " [",
          "-0.6248",
          ", ",
          "-0.5353",
          "]",
          ", ",
          italic("n"),
          " = ",
          1579L
        )
      )

    # Cohen's d and central
    set.seed(123)
    using_function4 <-
      suppressWarnings(
        ggstatsplot::subtitle_t_onesample(
          data = movies_long,
          x = length,
          test.value = 120,
          type = "p",
          effsize.type = "d",
          effsize.noncentral = FALSE,
          k = 2,
          conf.level = 0.50,
          conf.type = "perc",
          messages = TRUE
        )
      )

    set.seed(123)
    results4 <-
      ggplot2::expr(
        paste(
          NULL,
          italic("t"),
          "(",
          "1578",
          ") = ",
          "-23.05",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("d"),
          " = ",
          "-0.58",
          ", CI"["50%"],
          " [",
          "-0.60",
          ", ",
          "-0.56",
          "]",
          ", ",
          italic("n"),
          " = ",
          1579L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
    testthat::expect_identical(using_function2, results2)
    testthat::expect_identical(using_function3, results3)
    testthat::expect_identical(using_function4, results4)
  }
)

# non-parametric -----------------------------------------------------------

testthat::test_that(
  desc = "subtitle_t_onesample non-parametric works",
  code = {
    testthat::skip_on_cran()

    # ggstatsplot output
    set.seed(123)
    using_function <-
      ggstatsplot::subtitle_t_onesample(
        data = ToothGrowth,
        x = len,
        test.value = 20,
        type = "np",
        k = 4,
        messages = TRUE
      )

    # expected output
    set.seed(123)
    results <-
      ggplot2::expr(
        paste(
          NULL,
          "log"["e"](italic("V")),
          " = ",
          "6.6247",
          ", ",
          italic("p"),
          " = ",
          "0.3227",
          ", ",
          italic("r"),
          " = ",
          "-0.1264",
          ", CI"["95%"],
          " [",
          "-0.3805",
          ", ",
          "0.1545",
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
    testthat::skip_on_cran()

    # ggstatsplot output
    set.seed(123)
    using_function <-
      ggstatsplot::subtitle_t_onesample(
        data = anscombe,
        x = x1,
        test.value = 8,
        type = "r",
        k = 4,
        conf.level = 0.99,
        messages = TRUE
      )

    # expected output
    set.seed(123)
    results <-
      ggplot2::expr(
        paste(
          italic("M")[robust],
          " = ",
          "9.0000",
          ", CI"["99%"],
          " [",
          "6.0128",
          ", ",
          "11.6299",
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
    testthat::skip_on_cran()

    # ggstatsplot output
    set.seed(123)
    using_function <-
      ggstatsplot::subtitle_t_onesample(
        data = anscombe,
        x = x2,
        test.value = 8,
        type = "bf",
        messages = FALSE
      )

    # expected output
    set.seed(123)
    results <-
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "-0.80",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.71"
        )
      ))

    # testing overall call
    testthat::expect_identical(using_function, results)
  }
)
