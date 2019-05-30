context("subtitle_contingency_tab_paired")

# paired data without NAs and counts data -------------------------------------

testthat::test_that(
  desc = "paired subtitle_contingency_tab works - counts data without NAs",
  code = {
    testthat::skip_on_cran()

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
          counts = "Freq",
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
          italic("g")["Cohen"],
          " = ",
          "-0.33333",
          ", CI"["95%"],
          " [",
          "-0.49302",
          ", ",
          "-0.18628",
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
    testthat::skip_on_cran()

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
          italic("g")["Cohen"],
          " = ",
          "-0.342",
          ", CI"["90%"],
          " [",
          "-0.458",
          ", ",
          "-0.192",
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

# paired data 3-by-3  ---------------------------------------------

testthat::test_that(
  desc = "paired data 4-by-4",
  code = {
    testthat::skip_on_cran()
    set.seed(123)

    # making data
    Input <- ("
    Before        Pastafarian2   Discordiant2   Dudist2   Jedi2
    Pastafarian   7              0              23         0
    Discordiant   0              7               0        33
    Dudist        3              0               7         1
    Jedi          0              1               0         7
    ")

    # matrix
    matrix_df <- as.matrix(read.table(textConnection(Input),
      header = TRUE,
      row.names = 1
    ))

    # cleaning the factor levels
    df <- as.data.frame(as.table(matrix_df)) %>%
      dplyr::mutate(.data = ., Var2 = stringr::str_remove(Var2, "2"))

    # ggstatsplot output
    set.seed(123)
    subtitle1 <- suppressWarnings(ggstatsplot::subtitle_contingency_tab(
      data = df,
      main = Var1,
      condition = Var2,
      counts = "Freq",
      paired = TRUE,
      k = 4,
      conf.level = 0.99,
      conf.type = "basic",
      nboot = 50,
      messages = FALSE
    ))

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          NULL,
          italic(chi)^2,
          "(",
          "6",
          ") = ",
          "NaN",
          ", ",
          italic("p"),
          " = ",
          "NaN",
          ", ",
          italic("g")["Cohen"],
          " = ",
          "0.4344",
          ", CI"["99%"],
          " [",
          "0.3838",
          ", ",
          "0.5092",
          "]",
          ", ",
          italic("n"),
          " = ",
          89L
        )
      )

    # testing overall call
    testthat::expect_identical(subtitle1, results1)
  }
)
