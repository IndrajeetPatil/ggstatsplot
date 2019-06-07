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
        main = "am",
        legend.title = "Engine",
        conf.level = 0.99,
        k = 5
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
          "0.00000",
          ", ",
          "0.64272",
          "]",
          ", ",
          italic("n"),
          " = ",
          32L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
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
        main = Sex,
        counts = "Freq",
        k = 3
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
          "0.531",
          ", ",
          "0.615",
          "]",
          ", ",
          italic("n"),
          " = ",
          2201L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)

# dataframe with NA --------------------------------------------------------

testthat::test_that(
  desc = "works with dataframes with NAs",
  code = {
    testthat::skip_on_cran()

    # from function
    set.seed(123)
    using_function <-
      ggstatsplot::subtitle_onesample_proptest(
        data = ggplot2::msleep,
        main = vore
      )

    # expected
    expected <-
      ggplot2::expr(
        paste(
          NULL,
          italic(chi)^2,
          "(",
          "3",
          ") = ",
          "19.26",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("V")["Cramer"],
          " = ",
          "0.29",
          ", CI"["95%"],
          " [",
          "0.14",
          ", ",
          "0.41",
          "]",
          ", ",
          italic("n"),
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
    df <- tibble::tribble(
      ~x,
      "one"
    )

    # subtitle
    using_function1 <- ggstatsplot::subtitle_onesample_proptest(
      data = df,
      main = x
    )

    # expected output
    results1 <- ggplot2::expr(paste(italic("n"), " = ", 1L))

    # capturing message
    p_message <-
      capture.output(ggstatsplot::subtitle_onesample_proptest(
        data = df,
        main = x
      ))

    # testing overall call
    testthat::expect_identical(using_function1, results1)

    # testing message
    testthat::expect_match(p_message[1], "Proportion test will not be run")
  }
)
