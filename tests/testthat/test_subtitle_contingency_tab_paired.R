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


# paired data with NAs  ---------------------------------------------

testthat::test_that(
  desc = "paired subtitle_contingency_tab works - with NAs",
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

    # expanding the dataframe
    paired_data %<>%
      tidyr::uncount(data = ., weights = Freq)

    # introduce NAs
    # check that 2-by-2 doesn't produce continuity correction
    set.seed(123)
    paired_data %<>%
      purrr::map_df(
        .x = .,
        .f = ~ .[sample(
          x = c(TRUE, NA),
          prob = c(0.8, 0.2),
          size = length(.),
          replace = TRUE
        )]
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
          k = 3,
          conf.level = 0.90,
          conf.type = "perc",
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
          "8.895",
          ", ",
          italic("p"),
          " = ",
          "0.003",
          ", ",
          "log"["e"](OR),
          " = ",
          "-1.674",
          ", CI"["90%"],
          " [",
          "-3.068",
          ", ",
          "-0.578",
          "]",
          ", ",
          italic("n"),
          " = ",
          67L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)
