context("cor_tets_ci")
# using mtcars dataset
set.seed(123)
# 3 seconds ????
df1 <- ggstatsplot:::cor_tets_ci(
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
mtcars2 <- datasets::mtcars
# induce an NA
mtcars2[1, 1] <- NA
set.seed(123)
df2 <- ggstatsplot:::cor_tets_ci(
  data = mtcars2,
  x = hp,
  y = mpg,
  nboot = 125,
  conf.level = .99,
  conf.type = c("norm"),
  method = "kendall",
  continuity = TRUE,
  alternative = "less"
)


test_that("cor_tets_ci works", {
  set.seed(123)
  # testing 12 conditions
  testthat::expect_equal(df1$r, -0.7428125, tolerance = .00002)
  testthat::expect_equal(df1$conf.low, -0.9164006, tolerance = .00002)
  testthat::expect_equal(df1$conf.high, -0.5732568, tolerance = .00002)
  testthat::expect_equal(df1$statistic, -5.854742, tolerance = .00002)
  testthat::expect_equal(df1$`p-value`, 0.00000000239, tolerance = .00002)
  testthat::expect_match(as.character(df1$alternative), "less")
  testthat::expect_equal(df2$r, -0.7360461, tolerance = .00002)
  testthat::expect_equal(df2$conf.low, -0.9230962, tolerance = .00002)
  testthat::expect_equal(df2$conf.high, -0.5628487, tolerance = .00002)
  testthat::expect_equal(df2$statistic, -5.70882, tolerance = .00002)
  testthat::expect_equal(df2$`p-value`, 5.688118e-09, tolerance = .00002)
  testthat::expect_match(as.character(df2$alternative), "less")
})
