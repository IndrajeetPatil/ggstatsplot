# context ------------------------------------------------------------------
context(desc = "long_to_wide_converter")

# long_to_wide_converter works ---------------------------------------------

testthat::test_that(
  desc = "long_to_wide_converter works",
  code = {
    testthat::skip_on_cran()

    # setup
    set.seed(123)
    library(ggstatsplot)
    library(jmv)
    data("bugs", package = "jmv")

    # converting to long format
    data_bugs <- bugs %>%
      tibble::as_tibble(.) %>%
      tidyr::gather(., key, value, LDLF:HDHF)

    # ----------------------- data without NAs ------------------------------

    # within-subjects
    set.seed(123)
    df1 <- ggstatsplot:::long_to_wide_converter(
      data = iris_long,
      x = condition,
      y = value
    )

    testthat::expect_equal(dim(df1), c(150L, 5L))

    # between-subjects
    set.seed(123)
    df2 <- ggstatsplot:::long_to_wide_converter(
      data = mtcars,
      x = am,
      y = wt,
      paired = FALSE
    )

    testthat::expect_equal(dim(df2), c(19L, 3L))

    # -------------------------- data with NAs ------------------------------

    # within-subjects
    set.seed(123)
    df3 <- ggstatsplot:::long_to_wide_converter(
      data = data_bugs,
      x = key,
      y = value
    )

    testthat::expect_equal(dim(df3), c(88L, 5L))

    # between-subjects
    set.seed(123)
    df4 <- ggstatsplot:::long_to_wide_converter(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      paired = FALSE
    )

    testthat::expect_equal(dim(df4), c(26L, 5L))
  }
)
