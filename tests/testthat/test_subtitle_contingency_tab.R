context("subtitle_contigency_tab")

testthat::test_that(desc = "subtitle_contigency_tab works",
                    code = {
                      set.seed(123)
                      using_function1 <-
                        suppressWarnings(
                          ggstatsplot:::subtitle_contigency_tab(
                            data = Titanic_full,
                            main = Survived,
                            condition = Class,
                            stat.title = "Testing",
                            k = 5,
                            conf.level = .99,
                            conf.type = "basic",
                            nboot = 5,
                            messages = FALSE
                          )
                        )

                      results1 <-
                        ggplot2::expr(
                          paste(
                            "Testing",
                            italic(chi) ^ 2,
                            "(",
                            3L,
                            ") = ",
                            "190.40110",
                            ", ",
                            italic("p"),
                            " = ",
                            "< 0.001",
                            ", ",
                            italic(V),
                            " = ",
                            "0.29412",
                            ", 95% CI [",
                            "0.26560",
                            ", ",
                            "0.31106",
                            "]",
                            ", ",
                            italic("n"),
                            " = ",
                            2201
                          )
                        )

                      # testing overall, omega squared and bayes factor
                      testthat::expect_identical(using_function1, results1)
                      testthat::expect_identical(as.character(using_function1)[6],
                                                 as.character(results1)[6])
                      testthat::expect_identical(as.character(using_function1)[15],
                                                 as.character(results1)[15])
                    })
