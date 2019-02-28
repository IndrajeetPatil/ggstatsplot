context("subtitle_kw_nonparametric")

# data without NAs ----------------------------------------------------------

testthat::test_that(
  desc = "subtitle_kw_nonparametric works - data without NAs",
  code = {
    testthat::skip_on_cran()

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      ggstatsplot::subtitle_kw_nonparametric(
        data = ggstatsplot::movies_long,
        x = genre,
        y = length,
        k = 5,
        messages = TRUE
      )

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          NULL,
          italic(chi)^2,
          "(",
          "8",
          ") = ",
          "342.90144",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          eta["H"]^2,
          " = ",
          "0.21331",
          ", CI"["95%"],
          " [",
          "0.16934",
          ", ",
          "0.24848",
          "]",
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


# data with NAs -------------------------------------------------------------

testthat::test_that(
  desc = "subtitle_kw_nonparametric works - data with NAs",
  code = {
    testthat::skip_on_cran()

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      suppressWarnings(ggstatsplot::subtitle_kw_nonparametric(
        data = ggplot2::msleep,
        x = vore,
        y = sleep_cycle,
        k = 3,
        conf.level = 0.99,
        conf.type = "perc",
        messages = FALSE
      ))

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          NULL,
          italic(chi)^2,
          "(",
          "3",
          ") = ",
          "5.240",
          ", ",
          italic("p"),
          " = ",
          "0.155",
          ", ",
          eta["H"]^2,
          " = ",
          "0.083",
          ", CI"["99%"],
          " [",
          "-0.108",
          ", ",
          "0.458",
          "]",
          ", ",
          italic("n"),
          " = ",
          31L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)
