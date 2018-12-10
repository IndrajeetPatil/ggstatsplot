context("subtitle_t_parametric")

testthat::test_that(
  desc = "parametric t-test works (between-subjects without NAs)",
  code = {

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      suppressWarnings(
        ggstatsplot::subtitle_t_parametric(
          data = dplyr::filter(
            ggstatsplot::movies_long,
            genre == "Action" | genre == "Drama"
          ),
          x = genre,
          y = rating,
          effsize.type = "d",
          effsize.noncentral = TRUE,
          var.equal = TRUE,
          conf.level = .99,
          k = 5,
          messages = FALSE
        )
      )

    # expected output
    # this test will have to be changed with the next release of `effsize`
    # d here should be negative but is displayed as positive
    # this is a bug in effsize and has been fixed in the development version
    # (https://github.com/mtorchiano/effsize/commit/3561d93f9e9f5a61b3460ba120b316f7e4c3352f)
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          italic("t"),
          "(",
          "612.00000",
          ") = ",
          "-10.52948",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("d"),
          " = ",
          "0.81440",
          ", CI"["99%"],
          " [",
          "0.58009",
          ", ",
          "1.04814",
          "]",
          ", ",
          italic("n"),
          " = ",
          614L
        )
      )
    # testing overall call
    testthat::expect_equal(using_function1, results1)
  }
)

testthat::test_that(
  desc = "parametric t-test works (between-subjects with NAs)",
  code = {

    # for reproducibility
    set.seed(123)

    # loading the dataset
    data("bugs", package = "jmv")

    # preparing long format dataframe
    bugs_long <-
      tibble::as.tibble(x = bugs) %>%
      dplyr::select(.data = ., HDLF, HDHF) %>%
      tidyr::gather(data = ., "key", "value", convert = TRUE)

    # output from ggstatsplot helper subtitle
    subtitle <-
      ggstatsplot::subtitle_t_bayes(
        data = bugs_long,
        x = key,
        y = value,
        paired = TRUE
      )

    # expected
    expected <- ggplot2::expr(paste(
      italic("t"),
      "(",
      89,
      ") = ",
      "3.61",
      ", log"["e"],
      "(BF"["10"],
      ") = ",
      "3.8",
      ", Prior width = ",
      "0.707",
      ", ",
      italic("d"),
      " = ",
      "0.38",
      ", ",
      italic("n"),
      " = ",
      90L
    ))

    # testing overall call
    testthat::expect_identical(subtitle, expected)
  }
)
