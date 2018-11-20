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
          "165613.50000",
          ", ",
          italic(Z),
          " = ",
          "-3.20156",
          ", ",
          italic(" p"),
          " = ",
          "0.00137",
          ", ",
          italic("r"),
          " = ",
          "-0.08815",
          ", ",
          italic("n"),
          " = ",
          1319L
        )
      )

    # testing overall everything identical
    testthat::expect_identical(
      object = using_function1,
      expected = results1
    )

    # testing U statistic
    testthat::expect_identical(
      object = as.character(using_function1)[4],
      expected = as.character(results1)[4]
    )

    # testing Z statistic
    testthat::expect_identical(
      object = as.character(using_function1)[8],
      expected = as.character(results1)[8]
    )

    # testing sample size
    testthat::expect_identical(
      object = using_function1[20],
      expected = results1[20]
    )
  }
)
