context("t1way_ci")

testthat::test_that(
  desc = "t1way_ci works",
  code = {
    testthat::skip_on_cran()

    # normal
    set.seed(123)
    df1 <-
      ggstatsplot:::t1way_ci(
        data = dplyr::filter(.data = ggplot2::msleep, vore != "insecti"),
        x = vore,
        y = brainwt,
        nboot = 25,
        conf.level = .99,
        tr = 0.05,
        conf.type = c("norm")
      )

    # percentile
    set.seed(123)
    df2 <-
      suppressWarnings(ggstatsplot:::t1way_ci(
        data = dplyr::filter(.data = ggplot2::msleep, vore != "insecti"),
        x = vore,
        y = brainwt,
        tr = 0.1,
        nboot = 50,
        conf.level = 0.99,
        conf.type = "perc"
      ))

    # test normal CI
    testthat::expect_equal(df1$xi, 0.6537392, tolerance = 0.00002)
    testthat::expect_equal(df1$conf.low, -0.9077024, tolerance = 0.00002)
    testthat::expect_equal(df1$conf.high, 1.933281, tolerance = 0.00002)
    testthat::expect_equal(df1$F.value, 0.6146867, tolerance = 0.00002)
    testthat::expect_equal(df1$p.value, 0.5487093, tolerance = 0.00002)

    # test percentile CI
    testthat::expect_equal(df2$xi, 1.27404, tolerance = 0.00002)
    testthat::expect_equal(df2$conf.low, 0.211037, tolerance = 0.00002)
    testthat::expect_equal(df2$conf.high, 3.414511, tolerance = 0.00002)
    testthat::expect_equal(df2$F.value, 0.260884, tolerance = 0.00002)
    testthat::expect_equal(df2$p.value, 0.772501, tolerance = 0.00002)
  }
)
