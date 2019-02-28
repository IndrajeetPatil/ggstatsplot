context("subtitle_anova_parametric")

# parametric anova subtitles (without NAs) -----------------------------------

testthat::test_that(
  desc = "parametric anova subtitles work (without NAs)",
  code = {
    testthat::skip_on_cran()

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      ggstatsplot::subtitle_anova_parametric(
        data = ggstatsplot::movies_long,
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
          NULL,
          italic("F"),
          "(",
          "8",
          ",",
          "399.03535",
          ") = ",
          "28.41410",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          eta["p"]^2,
          " = ",
          "0.13123",
          ", CI"["95%"],
          " [",
          "0.09826",
          ", ",
          "0.15804",
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

# parametric anova subtitles (with NAs) --------------------------------------

testthat::test_that(
  desc = "parametric anova subtitles work (with NAs)",
  code = {
    # the expected result
    set.seed(123)
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
    set.seed(123)
    using_function1 <-
      suppressWarnings(
        ggstatsplot::subtitle_anova_parametric(
          data = ggplot2::msleep,
          x = vore,
          y = sleep_total,
          k = 3,
          effsize.type = "biased",
          partial = FALSE,
          conf.level = 0.95,
          messages = FALSE
        )
      )

    # expected output
    results1 <-
      ggplot2::expr(
        paste(
          NULL,
          italic("F"),
          "(",
          "3",
          ",",
          "16.586",
          ") = ",
          "1.405",
          ", ",
          italic("p"),
          " = ",
          "0.277",
          ", ",
          eta^2,
          " = ",
          "0.085",
          ", CI"["95%"],
          " [",
          "-0.008",
          ", ",
          "0.258",
          "]",
          ", ",
          italic("n"),
          " = ",
          76L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)

# parametric anova subtitles (partial omega) ----------------------------------

testthat::test_that(
  desc = "parametric anova subtitles with partial omega-squared",
  code = {

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      ggstatsplot::subtitle_anova_parametric(
        data = ggplot2::msleep,
        x = vore,
        y = sleep_rem,
        effsize.type = "partial_omega",
        k = 4,
        nboot = 10,
        messages = FALSE
      )

    # expected output
    results1 <-
      ggplot2::expr(
        paste(
          NULL,
          italic("F"),
          "(",
          "3",
          ",",
          "11.1010",
          ") = ",
          "2.6325",
          ", ",
          italic("p"),
          " = ",
          "0.1017",
          ", ",
          omega["p"]^2,
          " = ",
          "0.1438",
          ", CI"["95%"],
          " [",
          "-0.0241",
          ", ",
          "0.4012",
          "]",
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

# parametric anova subtitles (partial eta and NAs) --------------------------

testthat::test_that(
  desc = "parametric anova subtitles with partial eta-squared and data with NAs",
  code = {

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      ggstatsplot::subtitle_anova_parametric(
        data = ggplot2::msleep,
        x = vore,
        y = sleep_rem,
        var.equal = TRUE,
        effsize.type = "partial_eta",
        k = 4,
        nboot = 10,
        messages = FALSE
      )

    # expected output
    results1 <-
      ggplot2::expr(
        paste(
          NULL,
          italic("F"),
          "(",
          "3",
          ",",
          "52",
          ") = ",
          "4.1361",
          ", ",
          italic("p"),
          " = ",
          "0.0105",
          ", ",
          eta["p"]^2,
          " = ",
          "0.1926",
          ", CI"["95%"],
          " [",
          "0.0129",
          ", ",
          "0.3387",
          "]",
          ", ",
          italic("n"),
          " = ",
          56L
        )
      )

    # testing overall call
    testthat::expect_identical(
      object = using_function1,
      expected = results1
    )
  }
)

# checking non-partial variants ----------------------------------

testthat::test_that(
  desc = "parametric anova subtitles with partial eta-squared and data with NAs",
  code = {

    # ggstatsplot output
    # eta
    set.seed(123)
    using_function1 <-
      ggstatsplot::subtitle_anova_parametric(
        data = ggplot2::msleep,
        x = vore,
        y = sleep_rem,
        effsize.type = "biased",
        conf.level = 0.95,
        partial = FALSE,
        k = 4,
        nboot = 10,
        messages = FALSE
      )

    # omega
    set.seed(123)
    using_function2 <-
      ggstatsplot::subtitle_anova_parametric(
        data = ggplot2::msleep,
        x = vore,
        y = sleep_rem,
        effsize.type = "unbiased",
        partial = FALSE,
        k = 4,
        conf.level = 0.99,
        nboot = 10,
        messages = FALSE
      )

    # expected output
    # eta
    results1 <-
      ggplot2::expr(
        paste(
          NULL,
          italic("F"),
          "(",
          "3",
          ",",
          "11.1010",
          ") = ",
          "2.6325",
          ", ",
          italic("p"),
          " = ",
          "0.1017",
          ", ",
          eta^2,
          " = ",
          "0.1926",
          ", CI"["95%"],
          " [",
          "0.0319",
          ", ",
          "0.4387",
          "]",
          ", ",
          italic("n"),
          " = ",
          56L
        )
      )

    # omega
    results2 <-
      ggplot2::expr(
        paste(
          NULL,
          italic("F"),
          "(",
          "3",
          ",",
          "11.1010",
          ") = ",
          "2.6325",
          ", ",
          italic("p"),
          " = ",
          "0.1017",
          ", ",
          omega^2,
          " = ",
          "0.1438",
          ", CI"["99%"],
          " [",
          "NA",
          ", ",
          "0.3932",
          "]",
          ", ",
          italic("n"),
          " = ",
          56L
        )
      )

    # testing overall call
    # eta
    testthat::expect_identical(using_function1, results1)

    # omega
    testthat::expect_identical(using_function2, results2)
  }
)
