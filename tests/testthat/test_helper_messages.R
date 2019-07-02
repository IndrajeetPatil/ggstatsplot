# context ------------------------------------------------------------
context(desc = "helper_messages")

# grouped_message is working ---------------------------------------------

testthat::test_that(
  desc = "grouped_message is working",
  code = {
    testthat::skip_on_cran()

    testthat::expect_output(
      ggstatsplot:::grouped_message(),
      "Warning: Individual plots in the combined",
      fixed = TRUE
    )
  }
)

# effsize_ci_message is working ---------------------------------------------

testthat::test_that(
  desc = "effsize_ci_message is working",
  code = {
    testthat::skip_on_cran()

    testthat::expect_output(
      ggstatsplot:::effsize_ci_message(),
      "CI for effect size estimate was computed with",
      fixed = TRUE
    )

    testthat::expect_equal(
      readr::parse_number(capture.output(
        ggstatsplot:::effsize_ci_message(conf.level = 0.99)
      )),
      99L
    )
  }
)

# ggcorrmat_matrix_message is working ------------------------------------

testthat::test_that(
  desc = "ggcorrmat_matrix_message is working",
  code = {
    testthat::skip_on_cran()

    testthat::expect_output(
      ggstatsplot:::ggcorrmat_matrix_message(),
      "the upper triangle: p-values adjusted for multiple comparisons",
      fixed = TRUE
    )
  }
)

# palette_message is working ------------------------------------

testthat::test_that(
  desc = "palette_message is working",
  code = {
    testthat::skip_on_cran()

    testthat::expect_output(
      ggstatsplot:::palette_message(
        package = "RColorBrewer",
        palette = "Dark2",
        min_length = 20
      ),
      "No. of factor levels",
      fixed = TRUE
    )
  }
)

# normality_message is working ---------------------------------------------

testthat::test_that(
  desc = "normality_message is working",
  code = {
    testthat::skip_on_cran()

    # message
    testthat::expect_output(
      ggstatsplot::normality_message(x = iris$Sepal.Length, k = 4),
      "0.0102",
      fixed = TRUE
    )

    # stats results
    df <- ggstatsplot::normality_message(
      x = iris$Sepal.Length,
      k = 4,
      output = "stats"
    )

    df_broom <- broom::tidy(stats::shapiro.test(iris$Sepal.Length))

    testthat::expect_equal(df$p.value, df_broom$p.value, tolerance = 0.001)
    testthat::expect_equal(df$statistic, df_broom$statistic, tolerance = 0.001)
  }
)


# bartlett_message is working ---------------------------------------------

testthat::test_that(
  desc = "bartlett_message is working",
  code = {
    testthat::skip_on_cran()

    # without NA ------------------------------------------------------

    # message
    testthat::expect_output(
      ggstatsplot::bartlett_message(
        data = morley,
        x = Expt,
        y = Speed,
        k = 4
      ),
      "Note: Bartlett's test for homogeneity of variances for factor Expt: p-value = 0.0210",
      fixed = TRUE
    )

    # stats results
    df <- ggstatsplot::bartlett_message(
      data = morley,
      x = Expt,
      y = Speed,
      k = 4,
      output = "stats"
    )

    df_broom <-
      broom::tidy(stats::bartlett.test(formula = Speed ~ Expt, data = morley))

    testthat::expect_equal(df$p.value, df_broom$p.value, tolerance = 0.001)
    testthat::expect_equal(df$statistic, df_broom$statistic, tolerance = 0.001)
    testthat::expect_equal(df$parameter, df_broom$parameter)

    # with NA ------------------------------------------------------

    # message
    testthat::expect_output(
      ggstatsplot::bartlett_message(
        data = ggplot2::msleep,
        x = vore,
        y = sleep_rem,
        k = 4
      ),
      "Note: Bartlett's test for homogeneity of variances for factor vore: p-value = 0.0225",
      fixed = TRUE
    )

    # stats results
    df <- ggstatsplot::bartlett_message(
      data = ggplot2::msleep,
      x = vore,
      y = sleep_rem,
      k = 4,
      output = "stats"
    )

    df_broom <-
      broom::tidy(stats::bartlett.test(
        formula = sleep_rem ~ vore,
        data = ggplot2::msleep
      ))

    testthat::expect_equal(df$p.value, df_broom$p.value, tolerance = 0.001)
    testthat::expect_equal(df$statistic, df_broom$statistic, tolerance = 0.001)
    testthat::expect_equal(df$parameter, df_broom$parameter)

    # with dropped factor level -------------------------------------------------

    # drop a factor level
    msleep_short <- dplyr::filter(.data = ggplot2::msleep, vore != "omni")

    # message
    testthat::expect_output(
      ggstatsplot::bartlett_message(
        data = msleep_short,
        x = vore,
        y = sleep_rem,
        k = 4
      ),
      "Note: Bartlett's test for homogeneity of variances for factor vore: p-value = 0.0188",
      fixed = TRUE
    )

    # stats results
    df <- ggstatsplot::bartlett_message(
      data = msleep_short,
      x = vore,
      y = sleep_rem,
      k = 4,
      output = "stats"
    )

    df_broom <-
      broom::tidy(stats::bartlett.test(
        formula = sleep_rem ~ vore,
        data = msleep_short
      ))

    testthat::expect_equal(df$p.value, df_broom$p.value, tolerance = 0.001)
    testthat::expect_equal(df$statistic, df_broom$statistic, tolerance = 0.001)
    testthat::expect_equal(df$parameter, df_broom$parameter)
  }
)

# proptest_message is working ---------------------------------------------

testthat::test_that(
  desc = "proptest_message is working",
  code = {
    testthat::skip_on_cran()

    testthat::expect_output(
      ggstatsplot:::proptest_message(main = "am", condition = "cyl"),
      "Results from one-sample proportion tests",
      fixed = TRUE
    )
  }
)
