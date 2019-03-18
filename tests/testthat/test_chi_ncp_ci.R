context("chi_ncp_ci works")

testthat::test_that(
  desc = "chi_ncp_ci works",
  code = {
    testthat::skip_on_cran()
    set.seed(123)

    # expect errors
    testthat::expect_error(ggstatsplot:::chi_ncp_ci(2.3, 2, 2))
    testthat::expect_error(ggstatsplot:::chi_ncp_ci(-2.3, 2, 2))
    testthat::expect_error(ggstatsplot:::chi_ncp_ci(2.3, 2, 2, 0.002, 2))

    # checking extreme cases
    testthat::expect_warning(ggstatsplot:::chi_ncp_ci(2.3, 0.95, 2))
    testthat::expect_warning(ggstatsplot:::chi_ncp_ci(0, 0.95, 2))
    testthat::expect_warning(ggstatsplot:::chi_ncp_ci(1, 0.95, 50))
    testthat::expect_warning(ggstatsplot:::chi_ncp_ci(5, 0.99999, 5))

    # sensible values
    set.seed(123)
    df1 <- ggstatsplot:::chi_ncp_ci(20, 0.95, 1)
    df2 <- ggstatsplot:::chi_ncp_ci(20, 0.99, 1)
    df3 <- ggstatsplot:::chi_ncp_ci(22, 0.99, 2, 0.01, 0.99)

    # tests
    testthat::expect_equal(df1$Lower.Limit[[1]], 6.311008, tolerance = 0.001)
    testthat::expect_equal(df1$Upper.Limit[[1]], 41.37191, tolerance = 0.001)
    testthat::expect_equal(df2$Lower.Limit[[1]], 3.595979, tolerance = 0.001)
    testthat::expect_equal(df2$Upper.Limit[[1]], 49.67381, tolerance = 0.001)
    testthat::expect_equal(df3$Lower.Limit[[1]], 0.007512563, tolerance = 0.001)
    testthat::expect_equal(df3$Upper.Limit[[1]], 50.74284, tolerance = 0.001)
  }
)
