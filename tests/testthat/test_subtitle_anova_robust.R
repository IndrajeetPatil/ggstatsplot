context("subtitle_anova_robust")

testthat::test_that(desc = "subtitle_anova_robust works",
                    code = {
                      set.seed(123)
                      using_function1 <-
                        ggstatsplot:::subtitle_anova_robust(
                          data = dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.5),
                          x = genre,
                          y = length,
                          k = 5,
                          tr = 0.00025,
                          nboot = 2,
                          messages = FALSE
                        )

                      results1 <-
                        ggplot2::expr(
                          paste(
                            italic("F"),
                            "(",
                            5,
                            ",",
                            "53.64047",
                            ") = ",
                            "36.02148",
                            ", ",
                            italic("p"),
                            " = ",
                            "< 0.001",
                            ", ",
                            italic(xi),
                            " = ",
                            "0.58745",
                            ", 95% CI [",
                            "0.55162",
                            ", ",
                            "0.62561",
                            "]",
                            ", ",
                            italic("n"),
                            " = ",
                            1216L
                          )
                        )

                      # testing overall, omega squared and bayes factor
                      testthat::expect_identical(using_function1, results1)
                      testthat::expect_identical(as.character(using_function1)[8],
                                                 as.character(results1)[8])
                      testthat::expect_identical(as.character(using_function1)[18],
                                                 as.character(results1)[18])
                    })
