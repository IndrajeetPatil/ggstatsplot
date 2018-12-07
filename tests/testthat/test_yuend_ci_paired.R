context("test_yuend_ci_paired")

testthat::test_that(
  desc = "Yuen's test on trimmed means for dependent samples works",
  code = {
    testthat::skip_on_cran()

    # made up data
    mydata <-
      structure(list(
        time = structure(
          c(
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L
          ),
          .Label = c("test1", "test2"),
          class = "factor"
        ),
        grade = c(
          42.9,
          51.8,
          71.7,
          51.6,
          63.5,
          58,
          59.8,
          50.8,
          62.5,
          61.9,
          50.4,
          52.6,
          63,
          58.3,
          53.3,
          58.7,
          50.1,
          64.2,
          57.4,
          57.1,
          44.6,
          54,
          72.3,
          53.4,
          63.8,
          59.3,
          60.8,
          51.6,
          64.3,
          63.2,
          51.8,
          52.2,
          63,
          60.5,
          57.1,
          60.1,
          51.7,
          65.6,
          58.3,
          60.1
        )
      ),
      row.names = c(NA, -40L),
      class = "data.frame"
      )
    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      ggstatsplot:::yuend_ci(
        data = mydata,
        time,
        grade
      )


    # testing 5 conditions
    # dataframe without NAs
    testthat::expect_equal(using_function1$`t-value`, -5.27, tolerance = .001)
    testthat::expect_equal(using_function1$conf.low, 0.0952, tolerance = 0.0001)
    testthat::expect_equal(using_function1$conf.high, 0.248, tolerance = 0.001)
    testthat::expect_equal(using_function1$df, 15)
    testthat::expect_equal(using_function1$`p-value`, 0.0000945, tolerance = 0.000001)
  }
)
