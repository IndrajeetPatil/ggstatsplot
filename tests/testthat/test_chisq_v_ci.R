context("chisq_v_ci")

testthat::test_that(
  desc = "chisq_v_ci works",
  code = {
    testthat::skip_on_cran()

    # using sampled 25% Titanic_full dataset
    set.seed(123)
    testdata1 <-
      dplyr::sample_frac(tbl = ggstatsplot::Titanic_full, size = 0.25)

    # add a couple of NAs
    testdata2 <- testdata1
    testdata2[20, 5] <- NA
    testdata2[100, 3] <- NA

    # run function create outputs

    # dataframe without NAs
    set.seed(123)
    df1 <- ggstatsplot:::chisq_v_ci(
      data = testdata1,
      rows = Sex,
      cols = Survived,
      nboot = 12,
      conf.level = 0.90,
      conf.type = c("norm")
    )

    # dataframe with NAs
    # this also makes sure that the quoted arguments work
    set.seed(123)
    df2 <- ggstatsplot:::chisq_v_ci(
      data = testdata2,
      rows = "Sex",
      cols = "Survived",
      nboot = 12,
      conf.level = 0.90,
      conf.type = c("norm")
    )

    # testing 10 conditions
    set.seed(123)

    # dataframe without NAs
    testthat::expect_equal(df1$chi.sq, 114.9119, tolerance = .0002)
    testthat::expect_equal(df1$conf.low, 0.3805465, tolerance = 0.00002)
    testthat::expect_equal(df1$conf.high, 0.5513931, tolerance = 0.00002)
    testthat::expect_equal(df1$Cramer.V, 0.4570895, tolerance = 0.00002)
    testthat::expect_equal(df1$p.value, 8.227133e-27, tolerance = 0.00002)

    # dataframe with NAs
    testthat::expect_equal(df2$chi.sq, 112.9901, tolerance = 0.00002)
    testthat::expect_equal(df2$conf.low, 0.3591983, tolerance = 0.00002)
    testthat::expect_equal(df2$conf.high, 0.5200487, tolerance = 0.00002)
    testthat::expect_equal(df2$Cramer.V, 0.4540774, tolerance = 0.00002)
    testthat::expect_equal(df2$p.value, 2.168544e-26, tolerance = 0.00002)
  }
)
