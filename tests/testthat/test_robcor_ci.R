context("robcor_ci")

testthat::test_that(
  desc = "robcor_ci works",
  code = {
    # using mtcars dataset
    set.seed(123)
    #
    df1 <- ggstatsplot:::robcor_ci(
      data = datasets::mtcars,
      x = hp,
      y = mpg,
      beta = .01,
      nboot = 125,
      conf.level = .99,
      conf.type = c("norm")
    )
    mtcars2 <- datasets::mtcars

    # induce an NA
    mtcars2[1, 1] <- NA
    set.seed(123)

    # this also makes sure that the quoted arguments work
    df2 <- ggstatsplot:::robcor_ci(
      data = mtcars2,
      x = "hp",
      y = "mpg",
      beta = .01,
      nboot = 125,
      conf.level = .99,
      conf.type = c("norm")
    )

    # testing 8 conditions
    set.seed(123)

    # data without NAs
    testthat::expect_equal(df1$r, -0.8042457, tolerance = .00002)
    testthat::expect_equal(df1$conf.low, -0.9428293, tolerance = .00002)
    testthat::expect_equal(df1$conf.high, -0.6650366, tolerance = .00002)
    testthat::expect_equal(df1$`p-value`, 2.933186e-08, tolerance = .00002)

    # data with NAs
    testthat::expect_equal(df2$r, -0.8052814, tolerance = .00002)
    testthat::expect_equal(df2$conf.low, -0.9328399, tolerance = .00002)
    testthat::expect_equal(df2$conf.high, -0.686468, tolerance = .00002)
    testthat::expect_equal(df2$`p-value`, 4.677899e-08, tolerance = .00002)
  }
)
