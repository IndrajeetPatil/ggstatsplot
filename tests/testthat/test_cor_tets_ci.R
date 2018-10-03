context("cor_tets_ci")
# using mtcars dataset
set.seed(123)
# 3 seconds ????
df1<-ggstatsplot:::cor_tets_ci(data = datasets::mtcars,
                               x = hp,
                               y = mpg,
                               nboot = 125,
                               conf.level = .99,
                               conf.type = c("norm"),
                               method = "kendall",
                               continuity = TRUE,
                               alternative = "less")

test_that("cor_tets_ci works", {
  set.seed(123)
  # testing 5 conditions
  testthat::expect_equal(df1$r, -0.7428125, tolerance = .00002)
#  testthat::expect_equal(df1$conf.low, -0.1674259, tolerance = .00002)
#  testthat::expect_equal(df1$conf.high, 0.2684582, tolerance = .00002)
  testthat::expect_equal(df1$statistic, -5.854742, tolerance = .00002)
  testthat::expect_equal(df1$`p-value`, 0.00000000239, tolerance = .00002)
})
