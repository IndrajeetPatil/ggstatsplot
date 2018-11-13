context("subtitle_kw_nonparametric")

testthat::test_that(desc = "subtitle_kw_nonparametric works",
                    code = {
                      set.seed(123)
                      using_function1 <-
                        ggstatsplot:::subtitle_kw_nonparametric(
                          data = ggstatsplot::movies_long,
                          x = genre,
                          y = length,
                          k = 5,
                          messages = FALSE
                        )

                      results1 <-
                        ggplot2::expr(
                          paste(
                            "Kruskal-Wallis: ",
                            italic(chi) ^ 2,
                            "(",
                            5L,
                            ") = ",
                            "283.48849",
                            ", ",
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
                      testthat::expect_identical(as.character(using_function1)[2],
                                                 as.character(results1)[2])
                      testthat::expect_identical(as.character(using_function1)[7],
                                                 as.character(results1)[7])
                    })
