context("subtitle_t_bayes")

# between-subjects design -----------------------------------------------

testthat::test_that(
  desc = "subtitle_t_bayes works - between-subjects design",
  code = {
    testthat::skip_on_cran()

    # ggstatsplot output
    set.seed(123)
    using_function <-
      ggstatsplot::subtitle_t_bayes(
        data = dplyr::filter(movies_long, genre == "Action" | genre == "Drama"),
        x = genre,
        y = rating,
        bf.prior = .9,
        k = 5,
        messages = FALSE
      )

    # expected output
    set.seed(123)
    results <-
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "47.76267",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.90000"
        )
      ))

    # testing overall call
    testthat::expect_identical(using_function, results)
  }
)

# within-subjects design -----------------------------------------------

testthat::test_that(
  desc = "subtitle_t_bayes_paired works - within-subjects design",
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

    # ggstatsplot output
    set.seed(123)
    using_function <-
      ggstatsplot::subtitle_t_bayes(
        data = df_bird,
        x = type,
        y = length,
        bf.prior = 0.6,
        k = 5,
        paired = TRUE,
        messages = FALSE
      )

    # expected output
    set.seed(123)
    results <-
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "3.59201",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.60000"
        )
      ))

    # testing overall call
    testthat::expect_identical(using_function, results)
  }
)
