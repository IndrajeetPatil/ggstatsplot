context("subtitle_meta_ggcoefstats")

# subtitle from meta-analysis -------------------------------------------

testthat::test_that(
  desc = "subtitle_meta_ggcoefstats works",
  code = {

    # dataframe
    df <- tibble::tribble(
      ~estimate, ~std.error,
      0.111, 0.05,
      0.245, 0.111,
      0.8, 0.001,
      1.1, 0.2,
      0.03, 0.01
    )

    # subtitle output
    using_function1 <- ggstatsplot::subtitle_meta_ggcoefstats(
      data = df,
      k = 4,
      messages = FALSE,
      output = "subtitle"
    )

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          "Meta-analytic effect: ",
          beta,
          " = ",
          "0.4377",
          ", CI"["95%"],
          " [",
          "0.0423",
          ", ",
          "0.8331",
          "]",
          ", ",
          italic("z"),
          " = ",
          "2.1697",
          ", ",
          "se = ",
          "0.2017",
          ", ",
          italic("p"),
          " = ",
          "0.0300"
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)

    # tidy dataframe
    set.seed(123)
    tidy_df <- ggstatsplot::subtitle_meta_ggcoefstats(
      data = df,
      k = 4,
      messages = FALSE,
      output = "tidy"
    )

    # checking if the output is expected
    testthat::expect_equal(dim(tidy_df), c(1L, 7L))
    testthat::expect_equal(tidy_df$estimate, 0.4376927, tolerance = 0.0001)
    testthat::expect_equal(tidy_df$conf.low, 0.04231262, tolerance = 0.0001)
    testthat::expect_equal(tidy_df$conf.high, 0.8330728, tolerance = 0.0001)
  }
)


# checking meta-analysis results object -----------------------------------

testthat::test_that(
  desc = "checking meta-analysis results object",
  code = {

    # dataframe
    df <- tibble::tribble(
      ~estimate, ~std.error,
      0.111, 0.05,
      0.245, 0.111,
      0.8, 0.001,
      1.1, 0.2,
      0.03, 0.01
    )

    # output message
    message <- capture.output(ggstatsplot::subtitle_meta_ggcoefstats(
      data = df,
      messages = TRUE
    ))

    # test the ouput
    testthat::expect_identical(
      message[2],
      "Random-Effects Model (k = 5; tau^2 estimator: REML)"
    )
  }
)
