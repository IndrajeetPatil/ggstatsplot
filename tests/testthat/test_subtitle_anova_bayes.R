context("subtitle_anova_bayes")

# subtitle_anova_bayes works (unequal variance) ----------------------------

testthat::test_that(
  desc = "subtitle_anova_bayes works (unequal variance)",
  code = {
    set.seed(123)

    # ggstatsplot output
    using_function1 <-
      ggstatsplot::subtitle_anova_bayes(
        data = ggstatsplot::movies_long,
        x = genre,
        y = rating,
        effsize.type = "unbiased",
        k = 5,
        var.equal = FALSE,
        bf.prior = 0.8,
        nboot = 100
      )

    # expected output
    results1 <-
      ggplot2::expr(
        paste(
          italic("F"),
          "(",
          8,
          ",",
          "399.03535",
          ") = ",
          "28.41410",
          ", ",
          omega["p"]^2,
          " = ",
          "0.12673",
          ", log"["e"],
          "(BF"["10"],
          ") = ",
          "93.1",
          ", ",
          italic("r")["Cauchy"],
          " = ",
          "0.800",
          ", ",
          italic("n"),
          " = ",
          1579L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)

# subtitle_anova_bayes works (unequal variance) ----------------------------

testthat::test_that(
  desc = "subtitle_anova_bayes works (equal variance)",
  code = {

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      ggstatsplot::subtitle_anova_bayes(
        data = ggplot2::msleep,
        x = vore,
        y = brainwt,
        effsize.type = "biased",
        partial = FALSE,
        k = 4,
        var.equal = TRUE,
        bf.prior = 0.9,
        nboot = 20
      )

    # expected output
    results1 <-
      ggplot2::expr(
        paste(
          italic("F"),
          "(",
          3,
          ",",
          "47",
          ") = ",
          "1.0596",
          ", ",
          eta^2,
          " = ",
          "0.0633",
          ", log"["e"],
          "(BF"["10"],
          ") = ",
          "-2.0",
          ", ",
          italic("r")["Cauchy"],
          " = ",
          "0.900",
          ", ",
          italic("n"),
          " = ",
          51L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)
