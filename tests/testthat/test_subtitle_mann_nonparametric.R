context("subtitle_mann_nonparametric")

testthat::test_that(
  desc = "subtitle_mann_nonparametric works",
  code = {

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      ggstatsplot::subtitle_mann_nonparametric(
        data = dplyr::filter(movies_long, genre == "Action" | genre == "Drama"),
        x = genre,
        y = length,
        k = 5,
        messages = FALSE
      )

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          italic(U),
          " = ",
          "33136.50000",
          ", ",
          italic(Z),
          " = ",
          "-3.30155",
          ", ",
          italic(" p"),
          " = ",
          "< 0.001",
          ", ",
          italic("r"),
          " = ",
          "-0.13324",
          ", ",
          italic("n"),
          " = ",
          614L
        )
      )

    # testing overall everything identical
    testthat::expect_identical(using_function1, results1)
  }
)
