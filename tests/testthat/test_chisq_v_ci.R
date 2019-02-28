context("chisq_v_ci")

testthat::test_that(
  desc = "chisq_v_ci works",
  code = {
    testthat::skip_on_cran()

    # dataframe without NAs
    set.seed(123)
    df1 <- ggstatsplot:::chisq_v_ci(
      data = mtcars,
      rows = am,
      cols = cyl,
      nboot = 12,
      conf.level = 0.99,
      conf.type = c("norm")
    )

    # dataframe with NAs
    # this also makes sure that the quoted arguments work
    set.seed(123)
    df2 <- ggstatsplot:::chisq_v_ci(
      data = ggstatsplot::Titanic_full,
      rows = "Sex",
      cols = "Survived",
      nboot = 12,
      conf.level = 0.90,
      conf.type = c("norm")
    )

    # tests

    # dataframe without NAs
    testthat::expect_equal(df1$chi.sq, 8.740733, tolerance = 0.001)
    testthat::expect_equal(df1$conf.low, 0.1775918, tolerance = 0.001)
    testthat::expect_equal(df1$conf.high, 0.9614991, tolerance = 0.001)
    testthat::expect_equal(df1$Cramer.V, 0.5226355, tolerance = 0.001)
    testthat::expect_equal(df1$p.value, 0.01264661, tolerance = 0.001)

    # dataframe with NAs
    testthat::expect_equal(df2$chi.sq, 456.8742, tolerance = 0.001)
    testthat::expect_equal(df2$conf.low, 0.4194917, tolerance = 0.001)
    testthat::expect_equal(df2$conf.high, 0.4817644, tolerance = 0.001)
    testthat::expect_equal(df2$Cramer.V, 0.4556048, tolerance = 0.001)
    testthat::expect_equal(df2$p.value, 2.302151e-101, tolerance = 0.001)
  }
)
