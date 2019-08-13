# subtitle_contingency_tab ---------------------------------------------------

context("subtitle_contingency_tab")

# contingency tab - data without NAs -----------------------------------------

testthat::test_that(
  desc = "subtitle_contingency_tab works - data without NAs",
  code = {
    testthat::skip_on_cran()

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      suppressWarnings(ggstatsplot::subtitle_contingency_tab(
        data = ggstatsplot::Titanic_full,
        x = "Survived",
        y = Class,
        stat.title = "Testing",
        k = 5,
        conf.level = 0.99,
        conf.type = "basic",
        nboot = 5,
        messages = FALSE
      ))

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          "Testing",
          chi["Pearson"]^2,
          "(",
          "3",
          ") = ",
          "190.40110",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("V")["Cramer"],
          " = ",
          "0.29412",
          ", CI"["99%"],
          " [",
          "0.26922",
          ", ",
          "0.31248",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          2201L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)

    # with counts
    set.seed(123)
    using_function2 <- ggstatsplot::subtitle_contingency_tab(
      data = as.data.frame(Titanic),
      x = Sex,
      y = Survived,
      counts = "Freq",
      messages = FALSE
    )

    results2 <-
      ggplot2::expr(
        paste(
          NULL,
          chi["Pearson"]^2,
          "(",
          "1",
          ") = ",
          "456.87",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("V")["Cramer"],
          " = ",
          "0.46",
          ", CI"["95%"],
          " [",
          "0.41",
          ", ",
          "0.50",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          2201L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function2, results2)
  }
)

# contingency tab - data with NAs -----------------------------------------

testthat::test_that(
  desc = "subtitle_contingency_tab works - data with NAs",
  code = {
    testthat::skip_on_cran()

    # introduce NAs
    # check that 2-by-2 doesn't produce continuity correction
    set.seed(123)
    df <-
      purrr::map_df(
        .x = mtcars,
        .f = ~ .[sample(
          x = c(TRUE, NA),
          prob = c(0.8, 0.2),
          size = length(.),
          replace = TRUE
        )]
      ) %>%
      dplyr::filter(.data = ., cyl != "4")

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      suppressWarnings(ggstatsplot::subtitle_contingency_tab(
        data = df,
        x = am,
        y = "cyl",
        conf.level = .990,
        conf.type = "perc",
        nboot = 15,
        messages = FALSE,
        simulate.p.value = TRUE
      ))

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          NULL,
          chi["Pearson"]^2,
          "(",
          "NA",
          ") = ",
          "3.19",
          ", ",
          italic("p"),
          " = ",
          "0.107",
          ", ",
          italic("V")["Cramer"],
          " = ",
          "0.43",
          ", CI"["99%"],
          " [",
          "0.02",
          ", ",
          "0.83",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          17L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)

# subtitle_contingency_tab_paired ---------------------------------------------

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
          x = "response_before",
          y = response_after,
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
          chi["McNemar"]^2,
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
          italic("n")["pairs"],
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
          x = response_before,
          y = "response_after",
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
          chi["McNemar"]^2,
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
          italic("n")["pairs"],
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
      x = Var1,
      y = Var2,
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
          chi["McNemar"]^2,
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
          italic("n")["pairs"],
          " = ",
          89L
        )
      )

    # testing overall call
    testthat::expect_identical(subtitle1, results1)
  }
)

# subtitle_contingency_tab_gof -----------------------------------------

context("subtitle_contingency_tab_gof")

# checking subtitle (without counts) -----------------------------------------

testthat::test_that(
  desc = "Goodness of Fit subtitle_contingency_tab works without counts",
  code = {
    testthat::skip_on_cran()

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      ggstatsplot::subtitle_onesample_proptest(
        data = mtcars,
        x = "am",
        legend.title = "Engine",
        conf.level = 0.99,
        messages = FALSE,
        k = 5
      )

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          NULL,
          chi["gof"]^2,
          "(",
          "1",
          ") = ",
          "1.12500",
          ", ",
          italic("p"),
          " = ",
          "0.28884",
          ", ",
          italic("V")["Cramer"],
          " = ",
          "0.18750",
          ", CI"["99%"],
          " [",
          "-0.21551",
          ", ",
          "0.52301",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          32L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)

    # with counts
    set.seed(123)
    using_function2 <- ggstatsplot::subtitle_contingency_tab(
      data = as.data.frame(Titanic),
      x = Sex,
      counts = "Freq",
      messages = FALSE
    )

    results2 <-
      ggplot2::expr(
        paste(
          NULL,
          chi["gof"]^2,
          "(",
          "1",
          ") = ",
          "722.45",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("V")["Cramer"],
          " = ",
          "0.57",
          ", CI"["95%"],
          " [",
          "0.54",
          ", ",
          "0.61",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          2201L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function2, results2)
  }
)

# checking subtitle (with counts) -----------------------------------------

testthat::test_that(
  desc = "Goodness of Fit subtitle_contingency_tab works with counts",
  code = {
    testthat::skip_on_cran()

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      ggstatsplot::subtitle_onesample_proptest(
        data = as.data.frame(Titanic),
        x = Sex,
        counts = "Freq",
        k = 3,
        messages = FALSE
      )

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          NULL,
          chi["gof"]^2,
          "(",
          "1",
          ") = ",
          "722.454",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("V")["Cramer"],
          " = ",
          "0.573",
          ", CI"["95%"],
          " [",
          "0.541",
          ", ",
          "0.607",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          2201L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)

# dataframe with NA  and with ratio ----------------------------------------

testthat::test_that(
  desc = "works with dataframes with NAs and with ratio",
  code = {
    testthat::skip_on_cran()

    # from function
    set.seed(123)
    using_function <-
      ggstatsplot::subtitle_onesample_proptest(
        data = ggplot2::msleep,
        x = vore,
        ratio = c(0.2, 0.2, 0.3, 0.3),
        conf.type = "perc",
        messages = TRUE
      )

    # expected
    expected <-
      ggplot2::expr(
        paste(
          NULL,
          chi["gof"]^2,
          "(",
          "3",
          ") = ",
          "33.76",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("V")["Cramer"],
          " = ",
          "0.38",
          ", CI"["95%"],
          " [",
          "0.29",
          ", ",
          "0.51",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          76L
        )
      )

    # testing if these are equivalent
    testthat::expect_identical(using_function, expected)
  }
)

# checking edge cases --------------------------------------------------------

testthat::test_that(
  desc = "works even in edge cases",
  code = {
    testthat::skip_on_cran()
    set.seed(123)

    # creating a dataframe
    df <- dplyr::filter(mtcars, am == "0")

    # subtitle
    testthat::expect_null(ggstatsplot::subtitle_onesample_proptest(
      data = df,
      x = am
    ), NULL)
  }
)
