context(desc = "sort_xy")

testthat::test_that(
  desc = "sort_xy works as expected",
  code = {
    library(ggplot2)

    # without NAs
    set.seed(123)
    df1 <- ggstatsplot:::sort_xy(iris_long, condition, value, sort = "none")
    df2 <- ggstatsplot:::sort_xy(iris_long, condition, value, sort = "descending")
    df3 <- ggstatsplot:::sort_xy(iris_long, condition, value, sort = "ascending")

    testthat::expect_equal(
      levels(df1$condition),
      c("Sepal.Length", "Petal.Length", "Sepal.Width", "Petal.Width")
    )
    testthat::expect_equal(levels(df1$condition), levels(df2$condition))
    testthat::expect_equal(levels(df3$condition), rev(levels(df1$condition)))
    testthat::expect_equal(names(iris_long), names(df1))

    # with NAs
    set.seed(123)
    df4 <- ggstatsplot:::sort_xy(msleep, vore, brainwt, sort = "none")
    df5 <- ggstatsplot:::sort_xy(msleep, vore, brainwt, sort = "descending")
    df6 <- ggstatsplot:::sort_xy(msleep, vore, brainwt, sort = "ascending")

    testthat::expect_equal(
      levels(df4$vore),
      c("herbi", "omni", "carni", "insecti")
    )
    testthat::expect_equal(levels(df4$vore), levels(df5$vore))
    testthat::expect_equal(levels(df5$vore), rev(levels(df6$vore)))
    testthat::expect_equal(names(msleep), names(df5))
  }
)
