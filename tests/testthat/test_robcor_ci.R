context("robcor_ci")

testthat::test_that(
  desc = "robcor_ci works",
  code = {
    testthat::skip_on_cran()

    # using mtcars dataset
    set.seed(123)
    df1 <- ggstatsplot:::robcor_ci(
      data = datasets::mtcars,
      x = hp,
      y = mpg,
      beta = .01,
      nboot = 125,
      conf.level = .99,
      conf.type = c("norm")
    )

    # induce an NA
    mtcars2 <- datasets::mtcars
    mtcars2[1, 1] <- NA
    set.seed(123)

    # this also makes sure that the quoted arguments work
    df2 <-
      suppressWarnings(ggstatsplot:::robcor_ci(
        data = mtcars2,
        x = "hp",
        y = "mpg",
        beta = .01,
        nboot = 125,
        conf.level = .99,
        conf.type = c("basic")
      ))

    # data without NAs
    testthat::expect_equal(df1$estimate, -0.8042457, tolerance = .00002)
    testthat::expect_equal(df1$conf.low, -0.9428293, tolerance = .00002)
    testthat::expect_equal(df1$conf.high, -0.6650366, tolerance = .00002)
    testthat::expect_equal(df1$p.value, 2.933186e-08, tolerance = .00002)
    testthat::expect_equal(df1$statistic, -7.412179, tolerance = .00002)
    testthat::expect_identical(class(df1)[[1]], "tbl_df")

    # data with NAs
    testthat::expect_equal(df2$estimate, -0.8052814, tolerance = .00002)
    testthat::expect_equal(df2$conf.low, -0.9346235, tolerance = .00002)
    testthat::expect_equal(df2$conf.high, -0.6943768, tolerance = .00002)
    testthat::expect_equal(df2$p.value, 4.677899e-08, tolerance = .00002)
    testthat::expect_equal(df2$statistic, -7.314263, tolerance = .00002)
  }
)
