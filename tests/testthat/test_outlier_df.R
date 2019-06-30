context(desc = "outlier_df")

# outlier_df works ----------------------------------------------------

testthat::test_that(
  desc = "outlier_df works as expected",
  code = {
    testthat::skip_on_cran()
    set.seed(123)

    # dataframe with outlier column (data without NA)
    df1 <- ggstatsplot::outlier_df(
      data = morley,
      x = Expt,
      y = Speed,
      outlier.label = Run,
      outlier.coef = 2
    ) %>%
      dplyr::arrange(outlier)

    testthat::expect_equal(dim(df1), c(100L, 5L))
    testthat::expect_equal(dim(tidyr::drop_na(df1)), c(4L, 5L))

    # dataframe with outlier column (data with NA)
    df2 <- ggstatsplot::outlier_df(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      outlier.label = genus,
      outlier.coef = 3
    ) %>%
      dplyr::arrange(outlier)

    testthat::expect_equal(dim(df2), c(83L, 13L))
    testthat::expect_equal(dim(dplyr::filter(df2, !is.na(outlier))), c(4L, 13L))
  }
)
