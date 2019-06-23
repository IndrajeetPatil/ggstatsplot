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
        main = "Survived",
        condition = Class,
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
          "0.15228",
          ", ",
          "0.17109",
          "]",
          ", ",
          italic("n"),
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
      main = Sex,
      condition = Survived,
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
          "0.42",
          ", ",
          "0.49",
          "]",
          ", ",
          italic("n"),
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
        main = am,
        condition = "cyl",
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
          "0.05",
          ", ",
          "0.87",
          "]",
          ", ",
          italic("n"),
          " = ",
          17L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)
