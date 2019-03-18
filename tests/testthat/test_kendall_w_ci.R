context("Kendall's W ci")

testthat::test_that(
  desc = "`kendall_w_ci()` works",
  code = {
    set.seed(123)
    library(ggstatsplot)
    library(jmv, warn.conflicts = FALSE)
    data("bugs", package = "jmv")

    # dataframe with NAs
    data_bugs1 <- bugs %>%
      tibble::as_tibble(.) %>%
      tidyr::gather(., key, value, LDLF:HDHF)

    # expect error
    testthat::expect_error(ggstatsplot:::kendall_w_ci(
      data = data_bugs1,
      x = key,
      y = value,
      id.variable = Subject,
      conf.level = 0.90
    ))

    # proper exclusion of NAs
    data_bugs2 <- bugs %>%
      dplyr::filter(., !is.na(LDHF) &
        !is.na(LDLF) & !is.na(HDLF) & !is.na(HDHF)) %>%
      tibble::as_tibble(.) %>%
      tidyr::gather(., key, value, LDLF:HDHF)

    df1 <- ggstatsplot:::kendall_w_ci(
      data = data_bugs2,
      x = key,
      y = value,
      id.variable = Subject,
      conf.level = .999
    )

    # tests
    testthat::expect_equal(df1$estimate, 0.2114916, tolerance = 0.001)
    testthat::expect_equal(df1$conf.low, 0.06063263, tolerance = 0.001)
    testthat::expect_equal(df1$conf.high, 0.4297085, tolerance = 0.001)
    testthat::expect_is(df1, "tbl_df")

    # dataframe without NAs
    df2 <- ggstatsplot:::kendall_w_ci(
      data = iris_long,
      x = condition,
      y = value,
      id.variable = id
    )

    # tests
    testthat::expect_equal(df2$estimate, 0.9111111, tolerance = 0.001)
    testthat::expect_equal(df2$conf.low, 0.7390406, tolerance = 0.001)
    testthat::expect_equal(df2$conf.high, 1, tolerance = 0.001)
  }
)
