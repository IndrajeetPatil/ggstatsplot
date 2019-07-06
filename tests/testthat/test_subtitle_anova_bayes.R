context("subtitle_anova_bayes")

# subtitle_anova_bayes works (between-subjects) ----------------------------

testthat::test_that(
  desc = "subtitle_anova_bayes works (between-subjects)",
  code = {
    testthat::skip_on_cran()

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      ggstatsplot::subtitle_anova_bayes(
        data = ggstatsplot::movies_long,
        x = genre,
        y = rating,
        k = 5,
        bf.prior = 0.8
      )

    # expected output
    results1 <-
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "93.14228",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.80000"
        )
      ))

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)

# subtitle_anova_bayes works (within-subjects) ----------------------------

testthat::test_that(
  desc = "subtitle_anova_bayes works (within-subjects)",
  code = {
    testthat::skip_on_cran()

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      ggstatsplot::subtitle_anova_bayes(
        data = WRS2::WineTasting,
        x = Wine,
        y = Taste,
        paired = TRUE,
        k = 3
      )

    # expected output
    results1 <-
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "2.115",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.707"
        )
      ))

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)

# subtitle_anova_bayes works (within-subjects) - with NA ----------------------

testthat::test_that(
  desc = "subtitle_anova_bayes works (within-subjects) - with NA",
  code = {
    testthat::skip_on_cran()

    set.seed(123)
    library(jmv, warn.conflicts = FALSE)
    data("bugs", package = "jmv")

    # proper exclusion of NAs
    data_bugs <- bugs %>%
      tibble::as_tibble(.) %>%
      tidyr::gather(., key, value, LDLF:HDHF)

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      ggstatsplot::subtitle_anova_bayes(
        data = data_bugs,
        x = key,
        y = value,
        paired = TRUE,
        k = 3
      )

    # expected output
    results1 <-
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "21.040",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.707"
        )
      ))

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)
