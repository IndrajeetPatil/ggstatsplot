context("chisq_v_ci")

testthat::test_that(
  desc = "chisq_v_ci works",
  code = {

    # using sampled 25% Titanic_full dataset
    set.seed(123)
    testdata1 <- dplyr::sample_frac(tbl = ggstatsplot::Titanic_full, size = 0.25)

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
      conf.level = .90,
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
      conf.level = .90,
      conf.type = c("norm")
    )

    # testing 10 conditions
    set.seed(123)

    # dataframe without NAs
    testthat::expect_equal(
      object = df1$chi.sq,
      expected = 114.9119,
      tolerance = .0002
    )
    testthat::expect_equal(
      object = df1$conf.low,
      expected = 0.3805465,
      tolerance = .00002
    )
    testthat::expect_equal(
      object = df1$conf.high,
      expected = 0.5513931,
      tolerance = .00002
    )
    testthat::expect_equal(
      object = df1$`Cramer's V`,
      expected = 0.4570895,
      tolerance = .00002
    )
    testthat::expect_equal(
      object = df1$`p-value`,
      expected = 8.227133e-27,
      tolerance = .00002
    )

    # dataframe with NAs
    testthat::expect_equal(
      object = df2$chi.sq,
      expected = 112.9901,
      tolerance = .00002
    )
    testthat::expect_equal(
      object = df2$conf.low,
      expected = 0.3591983,
      tolerance = .00002
    )
    testthat::expect_equal(
      object = df2$conf.high,
      expected = 0.5200487,
      tolerance = .00002
    )
    testthat::expect_equal(
      object = df2$`Cramer's V`,
      expected = 0.4540774,
      tolerance = .00002
    )
    testthat::expect_equal(
      object = df2$`p-value`,
      expected = 2.168544e-26,
      tolerance = .00002
    )
  }
)
