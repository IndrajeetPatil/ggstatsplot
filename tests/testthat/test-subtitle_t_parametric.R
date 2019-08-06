context("subtitle_t_parametric")

# parametric t-test (between-subjects without NAs) ---------------------------

testthat::test_that(
  desc = "parametric t-test works (between-subjects without NAs)",
  code = {
    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      suppressWarnings(
        ggstatsplot::subtitle_t_parametric(
          data = dplyr::filter(
            ggstatsplot::movies_long,
            genre == "Action" | genre == "Drama"
          ),
          x = genre,
          y = rating,
          effsize.type = "d",
          effsize.noncentral = TRUE,
          var.equal = TRUE,
          conf.level = .99,
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
          italic("t"),
          "(",
          "612",
          ") = ",
          "-10.52948",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("d"),
          " = ",
          "-0.92473",
          ", CI"["99%"],
          " [",
          "-1.16064",
          ", ",
          "-0.68817",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          614L
        )
      )

    # testing overall call
    testthat::expect_equal(using_function1, results1)
  }
)

# parametric t-test (between-subjects with NAs) ------------------------------

testthat::test_that(
  desc = "parametric t-test works (between-subjects with NAs)",
  code = {
    testthat::skip_on_cran()

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      suppressWarnings(
        ggstatsplot::subtitle_t_parametric(
          data = dplyr::filter(
            ggstatsplot::movies_long,
            genre == "Action" | genre == "Drama"
          ),
          x = genre,
          y = rating,
          effsize.type = "g",
          effsize.noncentral = FALSE,
          var.equal = FALSE,
          conf.level = .90,
          k = 3,
          messages = FALSE
        )
      )

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          NULL,
          italic("t"),
          "(",
          "271.302",
          ") = ",
          "-9.275",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("g"),
          " = ",
          "-0.863",
          ", CI"["90%"],
          " [",
          "-1.014",
          ", ",
          "-0.713",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          614L
        )
      )

    # testing overall call
    testthat::expect_equal(using_function1, results1)
  }
)

# parametric t-test (within-subjects without NAs) ---------------------------

testthat::test_that(
  desc = "parametric t-test works (within-subjects without NAs)",
  code = {
    testthat::skip_on_cran()

    # output from ggstatsplot helper subtitle
    set.seed(123)
    subtitle <-
      suppressWarnings(ggstatsplot::subtitle_t_parametric(
        data = dplyr::filter(
          ggstatsplot::iris_long,
          condition %in% c("Sepal.Length", "Sepal.Width")
        ),
        x = condition,
        y = value,
        paired = TRUE,
        effsize.type = "g",
        k = 4,
        conf.level = 0.50
      ))

    # expected
    expected <- ggplot2::expr(
      paste(
        NULL,
        italic("t"),
        "(",
        "149",
        ") = ",
        "34.8152",
        ", ",
        italic("p"),
        " = ",
        "< 0.001",
        ", ",
        italic("g"),
        " = ",
        "2.8282",
        ", CI"["50%"],
        " [",
        "2.7224",
        ", ",
        "2.9710",
        "]",
        ", ",
        italic("n")["pairs"],
        " = ",
        150L
      )
    )

    # testing overall call
    testthat::expect_identical(subtitle, expected)
  }
)


# parametric t-test (within-subjects with NAs) ---------------------------

testthat::test_that(
  desc = "parametric t-test works (within-subjects with NAs)",
  code = {
    testthat::skip_on_cran()

    # output from ggstatsplot helper subtitle
    set.seed(123)
    subtitle <-
      ggstatsplot::subtitle_t_parametric(
        data = dplyr::filter(ggstatsplot::bugs_long, condition %in% c("HDHF", "HDLF")),
        x = condition,
        y = desire,
        paired = TRUE,
        effsize.type = "d",
        effsize.noncentral = TRUE,
        k = 3
      )

    # expected
    expected <- ggplot2::expr(
      paste(
        NULL,
        italic("t"),
        "(",
        "89",
        ") = ",
        "3.613",
        ", ",
        italic("p"),
        " = ",
        "< 0.001",
        ", ",
        italic("d"),
        " = ",
        "0.381",
        ", CI"["95%"],
        " [",
        "0.167",
        ", ",
        "0.597",
        "]",
        ", ",
        italic("n")["pairs"],
        " = ",
        90L
      )
    )

    # testing overall call
    testthat::expect_identical(subtitle, expected)
  }
)
