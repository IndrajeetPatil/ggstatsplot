context("subtitle_anova_bayes")

testthat::test_that(
  desc = "subtitle_anova_bayes works",
  code = {
    set.seed(123)

    # ggstatsplot output
    using_function1 <-
      ggstatsplot:::subtitle_anova_bayes(
        data = movies_long,
        x = genre,
        y = rating,
        effsize.type = "unbiased",
        k = 5,
        var.equal = FALSE,
        bf.prior = .8,
        nboot = 100,
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
          omega["p"]^2,
          " = ",
          "0.05539",
          ", log"["e"],
          "(BF"["10"],
          ") = ",
          "60.6",
          ", Prior width = ",
          "0.800",
          ", ",
          italic("n"),
          " = ",
          2433L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)
