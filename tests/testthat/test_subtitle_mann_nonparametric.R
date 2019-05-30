context("subtitle_mann_nonparametric")

# between-subjects design -----------------------------------------------

testthat::test_that(
  desc = "subtitle_mann_nonparametric works - between-subjects design",
  code = {
    testthat::skip_on_cran()

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
          italic(r),
          " = ",
          "-0.133",
          ", CI"["90%"],
          " [",
          "-0.195",
          ", ",
          "-0.074",
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
    testthat::skip_on_cran()

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
    testthat::expect_error(suppressWarnings(ggstatsplot::subtitle_mann_nonparametric(
      data = iris,
      x = Sepal.Length,
      y = Species
    )))

    # ggstatsplot output
    set.seed(123)
    using_function2 <-
      suppressWarnings(ggstatsplot::subtitle_mann_nonparametric(
        data = df_bird,
        x = type,
        y = length,
        k = 5,
        nboot = 25,
        conf.type = "perc",
        conf.level = 0.99,
        paired = TRUE,
        messages = TRUE
      ))

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
          italic(r),
          " = ",
          "-0.74978",
          ", CI"["99%"],
          " [",
          "-0.88156",
          ", ",
          "-0.59505",
          "]",
          ", ",
          italic("n"),
          " = ",
          16L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function2, results2)
  }
)
