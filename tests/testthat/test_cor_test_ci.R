context("cor_test_ci")

testthat::test_that(
  desc = "cor_test_ci works",
  code = {

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
    testthat::expect_equal(
      object = df1$r,
      expected = -0.7428125,
      tolerance = .00002
    )
    testthat::expect_equal(
      object = df1$conf.low,
      expected = -0.9164006,
      tolerance = .00002
    )
    testthat::expect_equal(
      object = df1$conf.high,
      expected = -0.5732568,
      tolerance = .00002
    )
    testthat::expect_equal(
      object = df1$statistic,
      expected = -5.854742,
      tolerance = .00002
    )
    testthat::expect_equal(
      object = df1$`p-value`,
      expected = 0.00000000239,
      tolerance = .00002
    )
    testthat::expect_match(
      object = as.character(df1$alternative),
      regexp = "less"
    )

    # dataset with NAs
    testthat::expect_equal(
      object = df2$r,
      expected = -0.7360461,
      tolerance = .00002
    )
    testthat::expect_equal(
      object = df2$conf.low,
      expected = -0.9230962,
      tolerance = .00002
    )
    testthat::expect_equal(
      object = df2$conf.high,
      expected = -0.5628487,
      tolerance = .00002
    )
    testthat::expect_equal(
      object = df2$statistic,
      expected = -5.70882,
      tolerance = .00002
    )
    testthat::expect_equal(
      object = df2$`p-value`,
      expected = 5.688118e-09,
      tolerance = .00002
    )
    testthat::expect_match(
      object = as.character(df2$alternative),
      regexp = "less"
    )
  }
)
