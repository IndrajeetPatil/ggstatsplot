context("subtitle_anova_parametric")

testthat::test_that(
  desc = "parametric anova subtitles work (without NAs)",
  code = {
    set.seed(123)

    # ggstatsplot output
    using_function1 <-
      ggstatsplot::subtitle_anova_parametric(
        data = movies_long,
        x = genre,
        y = rating,
        effsize.type = "partial_eta",
        k = 5,
        var.equal = FALSE,
        nboot = 10,
        messages = FALSE
      )

    # expected output
    results1 <-
      ggplot2::expr(
        paste(
          italic("F"),
          "(",
          5,
          ",",
          "135.09275",
          ") = ",
          "29.36078",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          eta["p"]^2,
          " = ",
          "0.05735",
          ", CI"["95%"],
          " [",
          "0.03932",
          ", ",
          "0.07442",
          "]",
          ", ",
          italic("n"),
          " = ",
          2433L
        )
      )

    # testing overall call, eta squared and upper CI
    testthat::expect_identical(
      object = using_function1,
      expected = results1
    )

    # testing eta squared
    testthat::expect_identical(
      object = as.character(using_function1)[16],
      expected = as.character(results1)[16]
    )

    # testing upper CI
    testthat::expect_identical(
      object = as.character(using_function1)[20],
      expected = as.character(results1)[20]
    )
  }
)

testthat::test_that(
  desc = "parametric anova subtitles work (with NAs)",
  code = {
    # the expected result
    r <-
      broom::tidy(
        stats::oneway.test(
          formula = sleep_total ~ vore,
          na.action = na.omit,
          subset = NULL,
          data = ggplot2::msleep
        )
      )

    # output from ggstatsplot helper subtitle
    subtitle <-
      suppressWarnings(
        ggstatsplot::subtitle_anova_parametric(
          data = ggplot2::msleep,
          x = vore,
          y = sleep_total,
          k = 3,
          messages = FALSE
        )
      )

    # extracting only the numbers and creating a tibble
    subtitle_vec <- num_parser(ggstats.obj = subtitle)

    # testing values

    # numerator degress of freedom
    testthat::expect_equal(r$`num df`[[1]], subtitle_vec[[1]])

    # denominator degress of freedom
    testthat::expect_equal(r$`denom df`[[1]], subtitle_vec[[2]], tolerance = 1e-3)

    # F-value
    testthat::expect_equal(r$statistic[[1]], subtitle_vec[[3]], tolerance = 1e-3)

    # p-value
    testthat::expect_equal(r$p.value[[1]], subtitle_vec[[4]], tolerance = 1e-3)
  }
)
