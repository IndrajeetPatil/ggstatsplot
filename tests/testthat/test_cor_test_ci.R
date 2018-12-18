context("cor_test_ci")

testthat::test_that(
  desc = "cor_test_ci works",
  code = {
    testthat::skip_on_cran()

    # using mtcars dataset
    set.seed(123)

    # dataset without NAs
    df1 <- ggstatsplot:::cor_test_ci(
      data = datasets::mtcars,
      x = hp,
      y = mpg,
      nboot = 125,
      conf.level = .99,
      conf.type = c("norm"),
      method = "kendall",
      continuity = TRUE,
      alternative = "less"
    )

    # dataset with NAs
    mtcars2 <- datasets::mtcars
    mtcars2[1, 1] <- NA
    set.seed(123)

    # this also makes sure that the quoted arguments work
    df2 <- ggstatsplot:::cor_test_ci(
      data = mtcars2,
      x = "hp",
      y = "mpg",
      nboot = 125,
      conf.level = .99,
      conf.type = c("norm"),
      method = "kendall",
      continuity = TRUE,
      alternative = "less"
    )

    # testing 12 conditions
    set.seed(123)

    # dataset without NAs
    testthat::expect_equal(df1$r, -0.7428125, tolerance = .00002)
    testthat::expect_equal(df1$conf.low, -0.9164006, tolerance = .00002)
    testthat::expect_equal(df1$conf.high, -0.5732568, tolerance = .00002)
    testthat::expect_equal(df1$statistic, -5.854742, tolerance = .00002)
    testthat::expect_equal(df1$p.value, 0.00000000239, tolerance = .00002)
    testthat::expect_match(as.character(df1$alternative), regexp = "less")

    # dataset with NAs
    testthat::expect_equal(df2$r, -0.7360461, tolerance = .00002)
    testthat::expect_equal(df2$conf.low, -0.9230962, tolerance = .00002)
    testthat::expect_equal(df2$conf.high, -0.5628487, tolerance = .00002)
    testthat::expect_equal(df2$statistic, -5.70882, tolerance = .00002)
    testthat::expect_equal(df2$p.value, 5.688118e-09, tolerance = .00002)
    testthat::expect_match(as.character(df2$alternative), regexp = "less")
  }
)
