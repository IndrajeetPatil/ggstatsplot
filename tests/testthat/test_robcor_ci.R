context("robcor_ci")

testthat::test_that(
  desc = "robcor_ci works",
  code = {
    testthat::skip_on_cran()
    testthat::skip_if_not(R.version$minor >= "6.0")

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

    # percentile CI
    set.seed(123)
    df3 <- suppressWarnings(ggstatsplot:::robcor_ci(
      data = ggplot2::msleep,
      x = brainwt,
      y = sleep_rem,
      beta = 0.1,
      nboot = 55,
      conf.level = 0.99,
      conf.type = "perc"
    ))

    # bca CI
    set.seed(123)
    df4 <- suppressWarnings(ggstatsplot:::robcor_ci(
      data = ggplot2::msleep,
      x = brainwt,
      y = sleep_rem,
      beta = 0.2,
      nboot = 100,
      conf.level = 0.90,
      conf.type = "bca"
    ))

    # data without NAs
    testthat::expect_equal(df1$estimate, -0.8042457, tolerance = 0.00001)
    testthat::expect_equal(df1$conf.low, -0.9367074, tolerance = 0.00001)
    testthat::expect_equal(df1$conf.high, -0.6795268, tolerance = 0.00001)
    testthat::expect_equal(df1$p.value, 2.933186e-08, tolerance = 0.00001)
    testthat::expect_equal(df1$statistic, -7.412179, tolerance = 0.00001)
    testthat::expect_identical(class(df1)[[1]], "tbl_df")

    # data with NAs
    testthat::expect_equal(df2$estimate, -0.8052814, tolerance = 0.00001)
    testthat::expect_equal(df2$conf.low, -0.9576768, tolerance = 0.00001)
    testthat::expect_equal(df2$conf.high, -0.717556, tolerance = 0.00001)
    testthat::expect_equal(df2$p.value, 4.677899e-08, tolerance = 0.00001)
    testthat::expect_equal(df2$statistic, -7.314263, tolerance = 0.00001)

    # percentile CI
    testthat::expect_equal(df3$estimate, -0.3956043, tolerance = 0.00001)
    testthat::expect_equal(df3$conf.low, -0.5488374, tolerance = 0.00001)
    testthat::expect_equal(df3$conf.high, -0.1557196, tolerance = 0.00001)
    testthat::expect_equal(df3$p.value, 0.005384018, tolerance = 0.00001)
    testthat::expect_equal(df3$statistic, -2.921448, tolerance = 0.00001)
    testthat::expect_equal(df3$conf, 0.99, tolerance = 0.001)
    testthat::expect_equal(df3$beta, 0.1, tolerance = 0.01)
    testthat::expect_equal(df3$nboot, 55L)
    testthat::expect_equal(df3$n, 48L)

    # bca CI
    testthat::expect_equal(df4$estimate, -0.4085762, tolerance = 0.00001)
    testthat::expect_equal(df4$conf.low, -0.5629717, tolerance = 0.00001)
    testthat::expect_equal(df4$conf.high, -0.1625011, tolerance = 0.00001)
  }
)
