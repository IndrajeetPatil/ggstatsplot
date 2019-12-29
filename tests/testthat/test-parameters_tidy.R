

testthat::test_that(
  desc = "grouped_message is working",
  code = {
    testthat::skip_on_cran()
    # setup
    library(lme4)
    library(parameters)
    set.seed(123)

    # model
    mm0 <-
      lme4::lmer(
        formula = scale(Reaction) ~ scale(Days) + (1 |
          Subject),
        data = sleepstudy
      )

    # model parameters
    df_parameters <- ggstatsplot:::parameters_tidy(mm0, ci = 0.95)
    df_tidy <-
      broom.mixed::tidy(mm0, effects = "fixed", conf.int = TRUE)

    # tests
    testthat::expect_equal(df_parameters$estimate, df_tidy$estimate, tolerance = 0.001)
    testthat::expect_equal(df_parameters$std.error, df_tidy$std.error, tolerance = 0.001)
    testthat::expect_equal(df_parameters$conf.low, df_tidy$conf.low, tolerance = 0.001)
    testthat::expect_equal(df_parameters$conf.high, df_tidy$conf.high, tolerance = 0.001)
    testthat::expect_equal(df_parameters$statistic, df_tidy$statistic, tolerance = 0.001)
  }
)
