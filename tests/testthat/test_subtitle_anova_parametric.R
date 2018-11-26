context("helpers_ggbetween_subtitles")

testthat::test_that(
  desc = "helpers_ggbetween_subtitles works",
  code = {
    set.seed(123)

    # ggstatsplot output
    using_function1 <-
      ggstatsplot::subtitle_anova_parametric(
        data = movies_long,
        x = genre,
        y = rating,
        effsize.type = "partial_eta",
        k = 5,
        var.equal = FALSE,
        nboot = 10,
        messages = FALSE
      )

    # expected output
    results1 <-
      ggplot2::expr(
        paste(
          italic("F"),
          "(",
          5,
          ",",
          "135.09275",
          ") = ",
          "29.36078",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          eta["p"]^2,
          " = ",
          "0.05735",
          ", CI"["95%"],
          " [",
          "0.03932",
          ", ",
          "0.07442",
          "]",
          ", ",
          italic("n"),
          " = ",
          2433L
        )
      )

    # testing overall call, eta squared and upper CI
    testthat::expect_identical(
      object = using_function1,
      expected = results1
    )

    # testing eta squared
    testthat::expect_identical(
      object = as.character(using_function1)[16],
      expected = as.character(results1)[16]
    )

    # testing upper CI
    testthat::expect_identical(
      object = as.character(using_function1)[20],
      expected = as.character(results1)[20]
    )
  }
)
