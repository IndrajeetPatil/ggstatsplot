context("subtitle_anova_nonparametric")

# between-subjects ----------------------------------------------------------

testthat::test_that(
  desc = "between-subjects - data with and without NAs",
  code = {
    testthat::skip_on_cran()

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      ggstatsplot::subtitle_anova_nonparametric(
        data = ggstatsplot::movies_long,
        x = genre,
        y = length,
        paired = FALSE,
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
          epsilon^2,
          " = ",
          "0.21730",
          ", CI"["95%"],
          " [",
          "0.17355",
          ", ",
          "0.25229",
          "]",
          ", ",
          italic("n"),
          " = ",
          1579L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)

    # ggstatsplot output
    set.seed(123)
    using_function2 <-
      suppressWarnings(ggstatsplot::subtitle_anova_nonparametric(
        data = ggplot2::msleep,
        x = vore,
        y = sleep_cycle,
        k = 3,
        paired = FALSE,
        conf.level = 0.99,
        conf.type = "perc",
        messages = FALSE
      ))

    # expected output
    set.seed(123)
    results2 <-
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
          epsilon^2,
          " = ",
          "0.175",
          ", CI"["99%"],
          " [",
          "0.003",
          ", ",
          "0.513",
          "]",
          ", ",
          italic("n"),
          " = ",
          31L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function2, results2)
  }
)

# within-subjects -------------------------------------------------------

testthat::test_that(
  desc = "within-subjects - data with and without NAs",
  code = {
    testthat::skip_on_cran()

    library(jmv)
    data("bugs", package = "jmv")

    # converting to long format
    data_bugs <- bugs %>%
      tibble::as_tibble(.) %>%
      tidyr::gather(., key, value, LDLF:HDHF)

    # ggstatsplot output
    set.seed(123)
    using_function1 <- ggstatsplot::subtitle_anova_nonparametric(
      data = data_bugs,
      x = key,
      y = value,
      k = 4,
      paired = TRUE,
      conf.level = 0.99,
      messages = FALSE
    )

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
          "55.8338",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("W")["Kendall"],
          " = ",
          "0.6148",
          ", CI"["99%"],
          " [",
          "0.4901",
          ", ",
          "0.7424",
          "]",
          ", ",
          italic("n"),
          " = ",
          88L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)

    # ggstatsplot output
    set.seed(123)
    using_function2 <- ggstatsplot::subtitle_anova_nonparametric(
      data = iris_long,
      x = condition,
      y = value,
      k = 3,
      paired = TRUE,
      conf.level = 0.90,
      messages = FALSE
    )

    # expected output
    set.seed(123)
    results2 <-
      ggplot2::expr(
        paste(
          NULL,
          italic(chi)^2,
          "(",
          "3",
          ") = ",
          "410.000",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("W")["Kendall"],
          " = ",
          "0.486",
          ", CI"["90%"],
          " [",
          "0.442",
          ", ",
          "0.533",
          "]",
          ", ",
          italic("n"),
          " = ",
          150L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function2, results2)
  }
)
