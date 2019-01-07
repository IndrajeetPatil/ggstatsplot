# context ------------------------------------------------------------
context(desc = "matrix_to_tibble")

# data with NAs ------------------------------------------------------

testthat::test_that(
  desc = "checking matrix_to_tibble works",
  code = {
    testthat::skip_on_cran()

    # creating correlation matrix
    set.seed(123)
    cor_df <- stats::cor(purrr::keep(.x = ggplot2::msleep, .p = is.numeric))

    # converting to tibble
    tbl_df <- ggstatsplot:::matrix_to_tibble(df = cor_df, var = "x")

    # checking correspondence between two objects
    testthat::expect_identical(class(tbl_df), c("tbl_df", "tbl", "data.frame"))
    testthat::expect_equal(cor_df[1, 6], tbl_df[1, 7][[1]], tolerance = 0.001)
    testthat::expect_equal(dim(tbl_df)[2], dim(cor_df)[2] + 1L)
    testthat::expect_equal(dim(tbl_df)[1], dim(cor_df)[1])
    testthat::expect_identical(colnames(tbl_df)[1], "x")
  }
)
