context(desc = "outlier_df")

# outlier_df works ----------------------------------------------------

testthat::test_that(
  desc = "outlier_df works as expected",
  code = {
    testthat::skip_on_cran()
    set.seed(123)

    # dataframe with outlier column
    df <- ggstatsplot::outlier_df(
      data = morley,
      x = Expt,
      y = Speed,
      outlier.label = Run,
      outlier.coef = 2
    ) %>%
      dplyr::arrange(outlier)

    testthat::expect_equal(dim(df), c(100L, 5L))
    testthat::expect_equal(dim(tidyr::drop_na(df)), c(4L, 5L))
  }
)
