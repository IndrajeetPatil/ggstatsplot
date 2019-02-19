context("subtitle_mann_nonparametric")

# between-subjects design -----------------------------------------------

testthat::test_that(
  desc = "subtitle_mann_nonparametric works - between-subjects design",
  code = {

    # ggstatsplot output
    set.seed(123)
    using_function <-
      ggstatsplot::subtitle_mann_nonparametric(
        data = dplyr::filter(movies_long, genre == "Action" | genre == "Drama"),
        x = genre,
        y = length,
        k = 3,
        conf.level = 0.90,
        messages = FALSE
      )

    # expected output
    set.seed(123)
    results <-
      ggplot2::expr(
        paste(
          NULL,
          "log"["e"](italic("W")),
          " = ",
          "10.408",
          ", ",
          italic("p"),
          " = ",
          "0.001",
          ", ",
          italic(r)["Spearman"],
          " = ",
          "0.133",
          ", CI"["90%"],
          " [",
          "0.068",
          ", ",
          "0.198",
          "]",
          ", ",
          italic("n"),
          " = ",
          614L
        )
      )

    # testing overall everything identical
    testthat::expect_identical(using_function, results)
  }
)

# within-subjects design -----------------------------------------------

testthat::test_that(
  desc = "subtitle_mann_nonparametric works - within-subjects design",
  code = {

    # made up data
    Input <- ("
              Bird   Typical  Odd
              A     -0.255   -0.324
              B     -0.213   -0.185
              C     -0.190   -0.299
              D     -0.185   -0.144
              E     -0.045   -0.027
              F     -0.025   -0.039
              G     -0.015   -0.264
              H      0.003   -0.077
              I      0.015   -0.017
              J      0.020   -0.169
              K      0.023   -0.096
              L      0.040   -0.330
              M      0.040   -0.346
              N      0.050   -0.191
              O      0.055   -0.128
              P      0.058   -0.182
              ")

    # creating a dataframe
    df_bird <- read.table(textConnection(Input), header = TRUE)

    # wide format dataframe
    df_bird_wide <- tibble::as_tibble(df_bird)

    # converting to long format
    df_bird %<>%
      tibble::as_tibble(x = .) %>%
      tidyr::gather(
        data = .,
        key = "type",
        value = "length",
        Typical:Odd
      )

    # expect error
    testthat::expect_error(ggstatsplot::subtitle_mann_nonparametric(
      data = iris,
      x = Sepal.Length,
      y = Species
    ))

    # wide format ----------------------------------------------------

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      suppressWarnings(ggstatsplot::subtitle_mann_nonparametric(
        data = df_bird_wide,
        x = Typical,
        y = Odd,
        k = 3,
        conf.level = 0.90,
        paired = TRUE,
        messages = FALSE
      ))

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          NULL,
          "log"["e"](italic("V")),
          " = ",
          "4.836",
          ", ",
          italic("p"),
          " = ",
          "0.003",
          ", ",
          italic(r)["Spearman"],
          " = ",
          "-0.559",
          ", CI"["90%"],
          " [",
          "-0.733",
          ", ",
          "-0.314",
          "]",
          ", ",
          italic("n"),
          " = ",
          16L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)

    # long format ----------------------------------------------------

    # ggstatsplot output
    set.seed(123)
    using_function2 <-
      ggstatsplot::subtitle_mann_nonparametric(
        data = df_bird,
        x = type,
        y = length,
        k = 5,
        conf.level = 0.99,
        paired = TRUE,
        messages = FALSE
      )

    # expected output
    set.seed(123)
    results2 <-
      ggplot2::expr(
        paste(
          NULL,
          "log"["e"](italic("V")),
          " = ",
          "2.30259",
          ", ",
          italic("p"),
          " = ",
          "0.00295",
          ", ",
          italic(r)["Spearman"],
          " = ",
          "0.55856",
          ", CI"["99%"],
          " [",
          "0.15124",
          ", ",
          "0.80373",
          "]",
          ", ",
          italic("n"),
          " = ",
          16
        )
      )

    # testing overall call
    testthat::expect_identical(using_function2, results2)
  }
)
