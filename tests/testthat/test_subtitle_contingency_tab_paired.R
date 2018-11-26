context("subtitle_contingency_tab_paired")

testthat::test_that(
  desc = "Paired subtitle_contingency_tab works",
  code = {

    # create data structure
    paired_data <-
      structure(
        list(
          response_before =
            structure(
              c(1L, 2L, 1L, 2L),
              .Label = c("no", "yes"),
              class = "factor"
            ),
          response_after = structure(
            c(1L, 1L, 2L, 2L),
            .Label = c("no", "yes"),
            class = "factor"
          ),
          Freq = c(65L, 25L, 5L, 5L)
        ),
        class = "data.frame",
        row.names = c(NA, -4L)
      )

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      suppressWarnings(
        ggstatsplot::subtitle_contingency_tab(
          data = paired_data,
          main = response_before,
          condition = response_after,
          paired = TRUE,
          counts = Freq,
          k = 5,
          messages = FALSE
        )
      )

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          NULL,
          italic(chi)^2,
          "(",
          1,
          ") = ",
          "13.33333",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          "log"["e"],
          "(OR) = ",
          "-1.60944",
          ", CI"["95%"],
          " [",
          "-2.817",
          ", ",
          "-0.631",
          "]",
          ", ",
          italic("n"),
          " = ",
          100L
        )
      )

    # testing chi-squared value
    testthat::expect_identical(
      object = as.character(using_function1)[7],
      expected = as.character(results1)[7]
    )

    # testing log odds ratio
    testthat::expect_identical(
      object = as.character(using_function1)[15],
      expected = as.character(results1)[15]
    )

    # testing sample size
    testthat::expect_identical(
      object = using_function1[24],
      expected = results1[24]
    )
  }
)
