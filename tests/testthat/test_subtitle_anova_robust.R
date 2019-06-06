context("subtitle_anova_robust")

# between-subjects -------------------------------------------------------

testthat::test_that(
  desc = "subtitle_anova_robust works - between-subjects",
  code = {
    testthat::skip_on_cran()

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      ggstatsplot::subtitle_anova_robust(
        data = dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.5),
        x = genre,
        y = length,
        paired = FALSE,
        k = 5,
        tr = 0.00025,
        nboot = 2,
        messages = FALSE
      )

    # expected output
    results1 <-
      ggplot2::expr(
        paste(
          NULL,
          italic("F"),
          "(",
          "8",
          ",",
          "201.17562",
          ") = ",
          "30.46450",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic(xi),
          " = ",
          "0.56962",
          ", CI"["95%"],
          " [",
          "0.48190",
          ", ",
          "0.58374",
          "]",
          ", ",
          italic("n"),
          " = ",
          790L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)

    # ggstatsplot output
    set.seed(123)
    using_function2 <-
      suppressWarnings(ggstatsplot::subtitle_anova_robust(
        data = dplyr::filter(ggplot2::msleep, vore != "insecti"),
        x = vore,
        y = sleep_total,
        paired = FALSE,
        k = 4,
        nboot = 15,
        conf.level = 0.99,
        conf.type = "basic",
        messages = TRUE
      ))

    # expected output
    results2 <-
      ggplot2::expr(
        paste(
          NULL,
          italic("F"),
          "(",
          "2",
          ",",
          "35.1708",
          ") = ",
          "0.2695",
          ", ",
          italic("p"),
          " = ",
          "0.7653",
          ", ",
          italic(xi),
          " = ",
          "0.1415",
          ", CI"["99%"],
          " [",
          "-0.0284",
          ", ",
          "0.2206",
          "]",
          ", ",
          italic("n"),
          " = ",
          71L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function2, results2)
  }
)

# within-subjects -------------------------------------------------------

testthat::test_that(
  desc = "subtitle_anova_robust works - within-subjects",
  code = {
    testthat::skip_on_cran()

    set.seed(123)
    library(jmv)
    data("bugs", package = "jmv")

    # converting to long format
    data_bugs <- bugs %>%
      tibble::as_tibble(.) %>%
      tidyr::gather(., key, value, LDLF:HDHF)

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      ggstatsplot::subtitle_anova_robust(
        data = data_bugs,
        x = key,
        y = value,
        k = 4,
        tr = 0.2,
        paired = TRUE
      )

    # expected output
    results1 <-
      ggplot2::expr(
        paste(
          italic("F"),
          "(",
          "2.7303",
          ",",
          "144.7051",
          ") = ",
          "20.9752",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("n"),
          " = ",
          88L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)

    # ggstatsplot output
    set.seed(123)
    using_function2 <- ggstatsplot::subtitle_anova_robust(
      data = iris_long,
      x = condition,
      y = value,
      k = 3,
      paired = TRUE
    )

    # expected output
    set.seed(123)
    results2 <-
      ggplot2::expr(
        paste(
          italic("F"),
          "(",
          "1.091",
          ",",
          "97.096",
          ") = ",
          "367.791",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("n"),
          " = ",
          150L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)
