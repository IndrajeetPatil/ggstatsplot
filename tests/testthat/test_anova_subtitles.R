context("subtitle_anova_parametric")

testthat::test_that(
  desc = "parametric anova subtitles work",
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
      suppressWarnings(ggstatsplot::subtitle_anova_parametric(
        data = ggplot2::msleep,
        x = vore,
        y = sleep_total,
        k = 3,
        messages = FALSE
      ))

    # extracting only the numbers and creating a tibble
    subtitle_vec <- num_parser(ggstats.obj = subtitle)

    # testing values

    # numerator degress of freedom
    testthat::expect_equal(
      expected = r$`num df`[[1]],
      object = subtitle_vec[[1]]
    )

    # denominator degress of freedom
    testthat::expect_equal(
      expected = r$`denom df`[[1]],
      object = subtitle_vec[[2]],
      tolerance = 1e-3
    )

    # F-value
    testthat::expect_equal(
      expected = r$statistic[[1]],
      object = subtitle_vec[[3]],
      tolerance = 1e-3
    )

    # p-value
    testthat::expect_equal(
      expected = r$p.value[[1]],
      object = subtitle_vec[[4]],
      tolerance = 1e-3
    )
  }
)
