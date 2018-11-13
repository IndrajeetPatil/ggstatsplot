context("subtitle_ggscatterstats")

testthat::test_that(desc = "subtitle_ggscatterstats works",
                    code = {
                      set.seed(123)
                      using_function1 <-
                        ggstatsplot:::subtitle_ggscatterstats(
                          data = movies_long,
                          x = rating,
                          y = length,
                          type = "nonparametric",
                          k = 5,
                          conf.level = .999,
                          conf.type = "perc",
                          nboot = 50,
                          messages = FALSE
                        )

                      results1 <-
                        ggplot2::expr(
                          paste(
                            "Spearman's ",
                            italic(rho),
                            "(",
                            2431,
                            ")",
                            " = ",
                            "0.43063",
                            ", 95% CI [",
                            "0.40225",
                            ", ",
                            "0.46936",
                            "], ",
                            italic("p"),
                            " = ",
                            "< 0.001",
                            ", ",
                            italic("n"),
                            " = ",
                            2433L
                          )
                        )

                      # testing overall, omega squared and bayes factor
                      testthat::expect_identical(using_function1, results1)
                      testthat::expect_identical(as.character(using_function1)[8],
                                                 as.character(results1)[8])
                      testthat::expect_identical(as.character(using_function1)[12],
                                                 as.character(results1)[13])
                    })
