context("lm_effsize_ci")

testthat::test_that(
  desc = "lm_effsize_ci works",
  code = {
    testthat::skip_on_cran()

    # for reproducibility
    set.seed(123)

    # creating lm object-1
    lmobject1 <- stats::lm(
      formula = mpg ~ hp * wt,
      data = mtcars
    )

    # induce an NA in the dataset
    mtcars2 <- datasets::mtcars
    mtcars2[1, 1] <- NA
    mtcars2[2, 4] <- NA
    mtcars2[3, 6] <- NA

    # creating lm object-2
    set.seed(123)
    lmobject2 <- stats::aov(
      formula = mpg ~ hp * wt,
      data = mtcars2
    )

    # creating lm object-3
    set.seed(123)
    lmobject3 <-
      stats::aov(
        formula = mpg ~ wt + qsec + Error(disp / am),
        data = mtcars
      )

    # model-1
    set.seed(123)
    df1 <- ggstatsplot::lm_effsize_ci(
      object = lmobject1,
      partial = FALSE,
      conf.level = .99,
      nboot = 100
    )

    # model-2
    set.seed(123)
    df2 <- ggstatsplot::lm_effsize_ci(
      object = lmobject2,
      partial = FALSE,
      conf.level = .99,
      nboot = 100
    )

    # model-3
    set.seed(123)
    df3 <- ggstatsplot::lm_effsize_ci(
      object = lmobject3,
      partial = FALSE,
      conf.level = .99,
      nboot = 100
    )

    # testing 8 conditions

    # model-1
    testthat::expect_equal(df1$etasq[1], 0.6024373, tolerance = .00002)
    testthat::expect_equal(df1$conf.low[1], 0.4675983, tolerance = .00002)
    testthat::expect_equal(df1$conf.high[1], 0.7821529, tolerance = .00002)
    testthat::expect_equal(df1$p.value[1], 1.227536e-12, tolerance = .00002)

    # model-2
    testthat::expect_equal(df2$p.value[2], 6.423958e-08, tolerance = .00002)
    testthat::expect_equal(df2$conf.low[2], 0.09086971, tolerance = .00002)
    testthat::expect_equal(df2$conf.high[2], 0.3558572, tolerance = .00002)
    testthat::expect_equal(df2$p.value[2], 6.423958e-08, tolerance = .00002)

    # model-3
    testthat::expect_equal(df3$etasq[2], 0.03034323, tolerance = .00002)
  }
)
