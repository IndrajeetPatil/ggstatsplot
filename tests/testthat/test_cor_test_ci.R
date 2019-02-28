context("cor_test_ci")

# kendall -------------------------------------------------------------

testthat::test_that(
  desc = "cor_test_ci works - kendall",
  code = {
    testthat::skip_on_cran()

    # using mtcars dataset
    set.seed(123)

    # dataset without NAs
    df1 <- ggstatsplot:::cor_test_ci(
      data = datasets::mtcars,
      x = hp,
      y = mpg,
      nboot = 25,
      conf.level = 0.90,
      conf.type = c("norm"),
      method = "kendall",
      continuity = TRUE,
      alternative = "less"
    )

    # dataset with NAs
    mtcars2 <- datasets::mtcars
    mtcars2[1, 1] <- NA

    # this also makes sure that the quoted arguments work
    set.seed(123)
    df2 <- ggstatsplot:::cor_test_ci(
      data = mtcars2,
      x = "hp",
      y = "mpg",
      nboot = 20,
      conf.level = 0.99,
      conf.type = "norm",
      method = "kendall",
      continuity = TRUE,
      alternative = "less"
    )

    # dataset without NAs
    testthat::expect_equal(df1$r, -0.7428125, tolerance = 0.00002)
    testthat::expect_equal(df1$conf.low, -0.8708747, tolerance = 0.00002)
    testthat::expect_equal(df1$conf.high, -0.6449002, tolerance = 0.00002)
    testthat::expect_equal(df1$statistic, -5.854742, tolerance = 0.00002)
    testthat::expect_equal(df1$p.value, 0.00000000239, tolerance = 0.00002)
    testthat::expect_match(as.character(df1$alternative), regexp = "less")

    # dataset with NAs
    testthat::expect_equal(df2$r, -0.7360461, tolerance = 0.00002)
    testthat::expect_equal(df2$conf.low, -0.8864601, tolerance = 0.00002)
    testthat::expect_equal(df2$conf.high, -0.5745845, tolerance = 0.00002)
    testthat::expect_equal(df2$statistic, -5.70882, tolerance = 0.00002)
    testthat::expect_equal(df2$p.value, 5.688118e-09, tolerance = 0.00002)
    testthat::expect_match(as.character(df2$alternative), regexp = "less")
  }
)

# pearson -------------------------------------------------------------

testthat::test_that(
  desc = "cor_test_ci works - pearson",
  code = {
    testthat::skip_on_cran()

    # using mtcars dataset
    set.seed(123)

    # dataset without NAs
    df1 <- ggstatsplot:::cor_test_ci(
      data = datasets::mtcars,
      x = hp,
      y = mpg,
      nboot = 20,
      conf.level = .99,
      conf.type = c("norm"),
      method = "pearson",
      continuity = TRUE,
      alternative = "less"
    )

    # dataset with NAs
    mtcars2 <- datasets::mtcars
    mtcars2[1, 1] <- NA

    # this also makes sure that the quoted arguments work
    set.seed(123)
    df2 <- ggstatsplot:::cor_test_ci(
      data = mtcars2,
      x = "hp",
      y = "mpg",
      nboot = 25,
      conf.level = .99,
      conf.type = c("norm"),
      method = "pearson",
      continuity = TRUE,
      alternative = "less"
    )

    # dataset without NAs
    testthat::expect_equal(df1$r, -0.7761684, tolerance = 0.00002)
    testthat::expect_equal(df1$conf.low, -0.9126434, tolerance = 0.00002)
    testthat::expect_equal(df1$conf.high, -0.6221413, tolerance = 0.00002)
    testthat::expect_equal(df1$statistic, -6.742389, tolerance = 0.00002)
    testthat::expect_equal(df1$p.value, 8.939176e-08, tolerance = 0.00002)
    testthat::expect_match(as.character(df1$alternative), regexp = "less")

    # dataset with NAs
    testthat::expect_equal(df2$r, -0.7774885, tolerance = 0.00002)
    testthat::expect_equal(df2$conf.low, -0.8958777, tolerance = 0.00002)
    testthat::expect_equal(df2$conf.high, -0.6446162, tolerance = 0.00002)
    testthat::expect_equal(df2$statistic, -6.657534, tolerance = 0.00002)
    testthat::expect_equal(df2$p.value, 1.33453e-07, tolerance = 0.00002)
    testthat::expect_match(as.character(df2$alternative), regexp = "less")
  }
)

# spearman -------------------------------------------------------------

testthat::test_that(
  desc = "cor_test_ci works - spearman",
  code = {
    testthat::skip_on_cran()

    # dataset without NAs
    set.seed(123)
    df1 <- suppressWarnings(ggstatsplot:::cor_test_ci(
      data = datasets::mtcars,
      x = hp,
      y = mpg,
      nboot = 25,
      conf.level = 0.99,
      conf.type = "perc",
      method = "spearman",
      continuity = TRUE,
      alternative = "greater"
    ))

    # dataset with NAs
    mtcars2 <- datasets::mtcars
    mtcars2[1, 1] <- NA

    # this also makes sure that the quoted arguments work
    set.seed(123)
    df2 <- suppressWarnings(ggstatsplot:::cor_test_ci(
      data = mtcars2,
      x = "hp",
      y = "mpg",
      nboot = 20,
      conf.level = 0.90,
      conf.type = "basic",
      method = "spearman",
      continuity = TRUE,
      alternative = "two.sided"
    ))

    # dataset without NAs + bca CI
    set.seed(123)
    df3 <- suppressWarnings(ggstatsplot:::cor_test_ci(
      data = datasets::mtcars,
      x = wt,
      y = mpg,
      nboot = 50,
      conf.level = 0.90,
      conf.type = "bca",
      method = "spearman"
    ))

    # dataset without NAs
    testthat::expect_equal(df1$r, -0.8946646, tolerance = 0.00002)
    testthat::expect_equal(df1$conf.low, -0.9507201, tolerance = 0.00002)
    testthat::expect_equal(df1$conf.high, -0.781459, tolerance = 0.00002)
    testthat::expect_equal(df1$statistic, 10337.29, tolerance = 0.00002)
    testthat::expect_equal(df1$p.value, 1, tolerance = 0.00002)
    testthat::expect_equal(df1$nboot, 25L)
    testthat::expect_match(as.character(df1$alternative), regexp = "greater")
    testthat::expect_identical(class(df1)[[1]], "tbl_df")

    # dataset with NAs
    testthat::expect_equal(df2$r, -0.8923343, tolerance = 0.00002)
    testthat::expect_equal(df2$conf.low, -0.9833273, tolerance = 0.00002)
    testthat::expect_equal(df2$conf.high, -0.8373902, tolerance = 0.00002)
    testthat::expect_equal(df2$statistic, 9385.978, tolerance = 0.00002)
    testthat::expect_equal(df2$p.value, 1.643287e-11, tolerance = 0.00002)
    testthat::expect_match(as.character(df2$alternative), regexp = "two.sided")

    # bca CI
    testthat::expect_equal(df3$r, -0.886422, tolerance = 0.00002)
    testthat::expect_equal(df3$conf.low, -0.9396298, tolerance = 0.00002)
    testthat::expect_equal(df3$conf.high, -0.7886005, tolerance = 0.00002)
  }
)
