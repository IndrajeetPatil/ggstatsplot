context("subtitle_friedman_nonparametric")

testthat::test_that(
  desc = "subtitle_friedman_nonparametric works",
  code = {
    testthat::skip_on_cran()

    library(jmv)
    data("bugs", package = "jmv")

    # converting to long format
    data_bugs <- bugs %>%
      tibble::as_tibble(.) %>%
      tidyr::gather(., key, value, LDLF:HDHF)

    # ggstatsplot output
    set.seed(123)
    using_function1 <- ggstatsplot::subtitle_friedman_nonparametric(
      data = data_bugs,
      x = key,
      y = value,
      k = 4,
      messages = FALSE
    )

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          "Friedman: ",
          italic(chi)^2,
          "(",
          3,
          ") = ",
          "55.8338",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("W")["Kendall"],
          " = ",
          "0.2115",
          ", ",
          italic("n"),
          " = ",
          88L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)
