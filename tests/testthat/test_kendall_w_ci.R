context("Kendall's W ci")

testthat::test_that(
  desc = "`kendall_w_ci()` works",
  code = {
    testthat::skip_on_cran()
    testthat::skip_if_not(R.version$minor >= "6.0")

    set.seed(123)
    library(ggstatsplot)
    library(jmv, warn.conflicts = FALSE)
    data("bugs", package = "jmv")

    # proper exclusion of NAs
    data_bugs <- bugs %>%
      dplyr::filter(., !is.na(LDHF) &
        !is.na(LDLF) & !is.na(HDLF) & !is.na(HDHF)) %>%
      tibble::as_tibble(.) %>%
      tidyr::gather(., key, value, LDLF:HDHF)

    set.seed(123)
    df1 <- ggstatsplot:::kendall_w_ci(
      data = data_bugs,
      nboot = 30,
      conf.level = .999
    )

    # tests
    testthat::expect_equal(df1$estimate, 0.1410648, tolerance = 0.001)
    testthat::expect_equal(df1$conf.low, 0.1141447, tolerance = 0.001)
    testthat::expect_equal(df1$conf.high, 0.1686084, tolerance = 0.001)
    testthat::expect_is(df1, "tbl_df")

    # dataframe without NAs
    set.seed(123)
    df2 <- ggstatsplot:::kendall_w_ci(iris_long, conf.type = "perc")

    # tests
    testthat::expect_equal(df2$estimate, 0.3366415, tolerance = 0.001)
    testthat::expect_equal(df2$conf.low, 0.3029868, tolerance = 0.001)
    testthat::expect_equal(df2$conf.high, 0.3688444, tolerance = 0.001)
  }
)
