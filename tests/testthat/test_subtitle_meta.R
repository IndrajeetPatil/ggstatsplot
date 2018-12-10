context("subtitle_meta")

# subtitle from meta-analysis -------------------------------------------

testthat::test_that(
  desc = "subtitle_meta works",
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

    # ggstatsplot output
    using_function1 <- ggstatsplot:::subtitle_meta(
      data = df,
      k = 4,
      messages = FALSE
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
  }
)
