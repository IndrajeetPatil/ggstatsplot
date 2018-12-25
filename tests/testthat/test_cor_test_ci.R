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
      nboot = 125,
      conf.level = .99,
      conf.type = c("norm"),
      method = "pearson",
      continuity = TRUE,
      alternative = "less"
    )

    # dataset with NAs
    mtcars2 <- datasets::mtcars
    mtcars2[1, 1] <- NA
    set.seed(123)

    # this also makes sure that the quoted arguments work
    set.seed(123)
    df2 <- ggstatsplot:::cor_test_ci(
      data = mtcars2,
      x = "hp",
      y = "mpg",
      nboot = 125,
      conf.level = .99,
      conf.type = c("norm"),
      method = "pearson",
      continuity = TRUE,
      alternative = "less"
    )

    # dataset without NAs
    testthat::expect_equal(df1$r, -0.7761684, tolerance = .00002)
    testthat::expect_equal(df1$conf.low, -0.8950344, tolerance = .00002)
    testthat::expect_equal(df1$conf.high, -0.6346684, tolerance = .00002)
    testthat::expect_equal(df1$statistic, -6.742389, tolerance = .00002)
    testthat::expect_equal(df1$p.value, 8.939176e-08, tolerance = .00002)
    testthat::expect_match(as.character(df1$alternative), regexp = "less")

    # dataset with NAs
    testthat::expect_equal(df2$r, -0.7774885, tolerance = .00002)
    testthat::expect_equal(df2$conf.low, -0.8892667, tolerance = .00002)
    testthat::expect_equal(df2$conf.high, -0.6545251, tolerance = .00002)
    testthat::expect_equal(df2$statistic, -6.657534, tolerance = .00002)
    testthat::expect_equal(df2$p.value, 1.33453e-07, tolerance = .00002)
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
      conf.level = .99,
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
      nboot = 100,
      conf.level = .90,
      conf.type = "basic",
      method = "spearman",
      continuity = TRUE,
      alternative = "two.sided"
    ))

    # dataset without NAs
    testthat::expect_equal(df1$r, -0.8946646, tolerance = .00002)
    testthat::expect_equal(df1$conf.low, -0.9509772, tolerance = .00002)
    testthat::expect_equal(df1$conf.high, -0.7030092, tolerance = .00002)
    testthat::expect_equal(df1$statistic, 10337.29, tolerance = .00002)
    testthat::expect_equal(df1$p.value, 1, tolerance = .00002)
    testthat::expect_equal(df1$nboot, 25L)
    testthat::expect_match(as.character(df1$alternative), regexp = "greater")
    testthat::expect_identical(class(df1)[[1]], "tbl_df")

    # dataset with NAs
    testthat::expect_equal(df2$r, -0.8923343, tolerance = .00002)
    testthat::expect_equal(df2$conf.low, -0.9989724, tolerance = .00002)
    testthat::expect_equal(df2$conf.high, -0.8408024, tolerance = .00002)
    testthat::expect_equal(df2$statistic, 9385.978, tolerance = .00002)
    testthat::expect_equal(df2$p.value, 1.643287e-11, tolerance = .00002)
    testthat::expect_equal(df2$nboot, 100L)
    testthat::expect_match(as.character(df2$alternative), regexp = "two.sided")
  }
)
