context("subtitle_kw_nonparametric")

testthat::test_that(
  desc = "subtitle_kw_nonparametric works",
  code = {

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      ggstatsplot::subtitle_kw_nonparametric(
        data = ggstatsplot::movies_long,
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
          "Kruskal-Wallis: ",
          italic(chi)^2,
          "(",
          8L,
          ") = ",
          "342.90144",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          eta["H"]^2,
          " = ",
          "0.21331",
          ", CI"["95%"],
          " [",
          "0.17162",
          ", ",
          "0.24782",
          "]",
          ", ",
          italic("n"),
          " = ",
          1579L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)
