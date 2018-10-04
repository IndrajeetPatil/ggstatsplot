context("chisq_v_ci")

# using sampled 25% Titanic_full dataset
set.seed(123)
testdata1 <- dplyr::sample_frac(ggstatsplot::Titanic_full, .25)

# add a couple of NA
testdata2 <- testdata1
testdata2[20, 5] <- NA
testdata2[100, 3] <- NA

# run function create outputs
set.seed(123)
df1 <- ggstatsplot:::chisq_v_ci(
  data = testdata1,
  rows = Sex,
  cols = Survived,
  nboot = 12,
  conf.level = .90,
  conf.type = c("norm")
)
set.seed(123)
df2 <- ggstatsplot:::chisq_v_ci(
  data = testdata2,
  rows = Sex,
  cols = Survived,
  nboot = 12,
  conf.level = .90,
  conf.type = c("norm")
)

# conduct tests
test_that("chisq_v_ci works", {
  set.seed(123)
  # testing 10 conditions
  testthat::expect_equal(df1$chi.sq, 114.9119, tolerance = .0002)
  testthat::expect_equal(df1$conf.low, 0.3805465, tolerance = .00002)
  testthat::expect_equal(df1$conf.high, 0.5513931, tolerance = .00002)
  testthat::expect_equal(df1$`Cramer's V`, 0.4570895, tolerance = .00002)
  testthat::expect_equal(df1$`p-value`, 8.227133e-27, tolerance = .00002)
  testthat::expect_equal(df2$chi.sq, 112.9901, tolerance = .00002)
  testthat::expect_equal(df2$conf.low, 0.3591983, tolerance = .00002)
  testthat::expect_equal(df2$conf.high, 0.5200487, tolerance = .00002)
  testthat::expect_equal(df2$`Cramer's V`, 0.4540774, tolerance = .00002)
  testthat::expect_equal(df2$`p-value`, 2.168544e-26, tolerance = .00002)
})
