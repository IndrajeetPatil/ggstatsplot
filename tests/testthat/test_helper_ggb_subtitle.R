context("helpers_ggbetween_subtitles")

testthat::test_that(desc = "helpers_ggbetween_subtitles works",
                    code = {
                      library(ggstatsplot)
                      set.seed(123)

                      using_function1 <- ggstatsplot:::subtitle_anova_parametric(
                        data = movies_long,
                        x = genre,
                        y = rating,
                        effsize.type = "partial_eta",
                        k = 5,
                        var.equal = FALSE,
                        nboot = 10,
                        messages = FALSE
                      )

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
                            ", p",
                            eta ^ 2,
                            " = ",
                            "0.05735",
                            ", 95% CI [",
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

                      # testing overall, eta squared and upper CI
                      testthat::expect_identical(using_function1, results1)
                      testthat::expect_identical(as.character(using_function1)[16], as.character(results1)[16])
                      testthat::expect_identical(as.character(using_function1)[20], as.character(results1)[20])
                    })
