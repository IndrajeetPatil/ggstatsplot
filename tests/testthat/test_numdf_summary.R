# context ------------------------------------------------------------
context(desc = "numdf_summary")

# data with NAs ---------------------------------------------

testthat::test_that(
  desc = "checking numdf_summary - with NAs",
  code = {

    # creating a dataframe with summaries
    set.seed(123)
    dat <- ggstatsplot:::numdf_summary(ggplot2::msleep)

    # checking values
    testthat::expect_equal(dat$n_min[1], 0.000140, tolerance = 1e-3)
    testthat::expect_equal(dat$n_median[1], 3.50, tolerance = 1e-2)
    testthat::expect_equal(dat$n_max[1], 6654L)
  }
)


# data without NAs ---------------------------------------------

testthat::test_that(
  desc = "checking numdf_summary - without NAs",
  code = {

    # creating a dataframe with summaries
    set.seed(123)
    dat <- ggstatsplot:::numdf_summary(mtcars)

    # checking values
    testthat::expect_equal(dat$n_min[1], 0L)
    testthat::expect_equal(dat$n_median[1], 4L)
    testthat::expect_equal(dat$n_max[1], 472L)
  }
)
