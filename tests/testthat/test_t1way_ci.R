context("t1way_ci")

testthat::test_that(
  desc = "t1way_ci works",
  code = {
    testthat::skip_on_cran()
    testthat::skip_if_not(R.version$minor >= "6.0")

    # normal
    set.seed(123)
    df1 <-
      ggstatsplot:::t1way_ci(
        data = dplyr::filter(.data = ggplot2::msleep, vore != "insecti"),
        x = vore,
        y = brainwt,
        nboot = 25,
        conf.level = 0.99,
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

    # bca
    set.seed(123)
    df3 <-
      suppressWarnings(ggstatsplot:::t1way_ci(
        data = dplyr::filter(.data = ggplot2::msleep, vore != "insecti"),
        x = vore,
        y = brainwt,
        nboot = 50,
        conf.level = 0.99,
        tr = 0.05,
        conf.type = c("bca")
      ))

    # test normal CI
    testthat::expect_equal(df1$xi, 0.7639015, tolerance = 0.00002)
    testthat::expect_equal(df1$conf.low, 0.06259349, tolerance = 0.00002)
    testthat::expect_equal(df1$conf.high, 1.838343, tolerance = 0.00002)
    testthat::expect_equal(df1$F.value, 0.6146867, tolerance = 0.00002)
    testthat::expect_equal(df1$p.value, 0.5487093, tolerance = 0.00002)

    # test percentile CI
    testthat::expect_equal(df2$xi, 1.452066, tolerance = 0.00002)
    testthat::expect_equal(df2$conf.low, 0.1435678, tolerance = 0.00002)
    testthat::expect_equal(df2$conf.high, 2.357306, tolerance = 0.00002)
    testthat::expect_equal(df2$F.value, 0.260884, tolerance = 0.00002)
    testthat::expect_equal(df2$p.value, 0.772501, tolerance = 0.00002)

    # test bca
    testthat::expect_equal(df3$xi, 0.5664255, tolerance = 0.00002)
    testthat::expect_equal(df3$conf.low, 0.2904682, tolerance = 0.00002)
    testthat::expect_equal(df3$conf.high, 1.724382, tolerance = 0.00002)
  }
)
