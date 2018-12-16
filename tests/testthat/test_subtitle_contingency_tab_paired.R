context("subtitle_contingency_tab_paired")

# paired data without NAs and counts data -------------------------------------

testthat::test_that(
  desc = "paired subtitle_contingency_tab works - counts data without NAs",
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
          "1",
          ") = ",
          "13.33333",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          "log"["e"](OR),
          " = ",
          "-1.60944",
          ", CI"["95%"],
          " [",
          "-2.81683",
          ", ",
          "-0.63132",
          "]",
          ", ",
          italic("n"),
          " = ",
          100L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)
