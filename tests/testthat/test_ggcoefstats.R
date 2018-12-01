context("ggcoefstats")

# stats::lm --------------------------------------------------

testthat::test_that(
  desc = "ggcoefstats with lm model",
  code = {
    set.seed(123)

    # model
    mod <- stats::lm(data = mtcars, formula = wt ~ mpg * am)

    # plot
    p <-
      ggstatsplot::ggcoefstats(x = mod,
                               conf.level = 0.99,
                               exclude.intercept = FALSE,
                               k = 3)

    # tidy dataframe from the function
    tidy_df <- p$plot_env$tidy_df

    # dataframe from `broom` package
    broom_df <- broom::tidy(x = mod,
                            conf.int = TRUE,
                            conf.level = 0.99)

    testthat::expect_equal(tidy_df$estimate, broom_df$estimate, tolerance = 1e-3)
    testthat::expect_equal(tidy_df$std.error, broom_df$std.error, tolerance = 1e-3)
    testthat::expect_equal(tidy_df$conf.low, broom_df$conf.low, tolerance = 1e-3)
    testthat::expect_equal(tidy_df$conf.high, broom_df$conf.high, tolerance = 1e-3)
    testthat::expect_equal(tidy_df$p.value, broom_df$p.value, tolerance = 1e-3)

    testthat::expect_identical(tidy_df$significance, c("***", "***", "*", "ns"))
    testthat::expect_identical(tidy_df$p.value.formatted,
                               c("< 0.001", "< 0.001", "0.014", "0.064"))
  })
