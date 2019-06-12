context("Kendall's W ci")

testthat::test_that(
  desc = "`kendall_w_ci()` works",
  code = {
    testthat::skip_on_cran()

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
      conf.type = "norm",
      nboot = 30,
      conf.level = .999
    )

    # tests
    testthat::expect_equal(df1$estimate, 0.1410648, tolerance = 0.001)
    testthat::expect_equal(df1$conf.low, 0.1141447, tolerance = 0.001)
    testthat::expect_equal(df1$conf.high, 0.1686084, tolerance = 0.001)
    testthat::expect_is(df1, "tbl_df")

    set.seed(123)
    testthat::expect_error(ggstatsplot:::kendall_w_ci(
      data = data_bugs,
      conf.type = "bca",
      nboot = 10,
      conf.level = 0.50
    ))

    set.seed(123)
    df3 <- suppressWarnings(ggstatsplot:::kendall_w_ci(
      data = data_bugs,
      conf.type = "basic",
      nboot = 15,
      conf.level = .999
    ))

    # tests
    testthat::expect_equal(df3$estimate, 0.1410648, tolerance = 0.001)
    testthat::expect_equal(df3$conf.low, 0.1244446, tolerance = 0.001)
    testthat::expect_equal(df3$conf.high, 0.1598686, tolerance = 0.001)

    # dataframe without NAs
    set.seed(123)
    df4 <- ggstatsplot:::kendall_w_ci(iris_long, conf.type = "perc")

    # tests
    testthat::expect_equal(df4$estimate, 0.3366415, tolerance = 0.001)
    testthat::expect_equal(df4$conf.low, 0.3029868, tolerance = 0.001)
    testthat::expect_equal(df4$conf.high, 0.3688444, tolerance = 0.001)

    testthat::expect_error(ggstatsplot:::kendall_w_ci(
      data = data_bugs,
      conf.type = "bca"
    ))
  }
)
