context("subtitle_anova_parametric_repeated")

# parametric repeated anova subtitles (basic) -----------------------------------

testthat::test_that(
  desc = "parametric anova subtitles work (without NAs)",
  code = {
    testthat::skip_on_cran()

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      subtitle_anova_parametric_repeated(
        data = iris_long,
        x = condition,
        y = value,
        id.variable = id
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
          "447",
          ") = ",
          "776.32",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          omega^2,
          " = ",
          "0.71",
          ", CI"["95%"],
          " [",
          "0.77",
          ", ",
          "0.82",
          "]",
          ", ",
          italic("n"),
          " = ",
          150
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)

# parametric repeated anova subtitles (with diff effect and confidence) --------------------------

testthat::test_that(
  desc = "parametric anova subtitles work ",
  code = {
    testthat::skip_on_cran()

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      subtitle_anova_parametric_repeated(
        data = iris_long,
        x = condition,
        y = value,
        id.variable = id,
        effsize.type = "biased",
        conf.level = .99
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
          "447",
          ") = ",
          "776.32",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          eta["p"]^2,
          " = ",
          "0.84",
          ", CI"["99%"],
          " [",
          "0.81",
          ", ",
          "0.86",
          "]",
          ", ",
          italic("n"),
          " = ",
          150
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)


# parametric repeated anova subtitles (with incorrect effsize) --------------------------

testthat::test_that(
  desc = "parametric anova subtitles work ",
  code = {
    testthat::skip_on_cran()

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      subtitle_anova_parametric_repeated(
        data = iris_long,
        x = condition,
        y = value,
        id.variable = id,
        effsize.type = "bogus",
        conf.level = .99
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
          "447",
          ") = ",
          "776.32",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          omega^2,
          " = ",
          "0.71",
          ", CI"["99%"],
          " [",
          "0.76",
          ", ",
          "0.82",
          "]",
          ", ",
          italic("n"),
          " = ",
          150
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)





# parametric repeated anova subtitles (catch bad data) --------------------------

testthat::test_that(
  desc = "parametric anova subtitles work ",
  code = {
    testthat::skip_on_cran()

    # ggstatsplot output
    set.seed(123)
    # fake a data entry mistake
    iris_long[5,3] <- "Sepal.Width"
    testthat::expect_error(
      subtitle_anova_parametric_repeated(
        data = iris_long,
        x = condition,
        y = value,
        id.variable = id
      )
    )
  }
)

