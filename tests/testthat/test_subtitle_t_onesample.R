context("subtitle_t_onesample")

testthat::test_that(
  desc = "subtitle_t_onesample works",
  code = {

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      suppressWarnings(
        ggstatsplot::subtitle_t_onesample(
          data = movies_long,
          x = length,
          test.value = 120,
          type = "p",
          k = 5,
          messages = FALSE
        )
      )

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          italic("t"),
          "(",
          2432,
          ") = ",
          "-28.74823",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("d"),
          " = ",
          "-0.58283",
          ", 95% CI [",
          "-0.62575",
          ", ",
          "-0.53980",
          "]",
          ", ",
          italic("n"),
          " = ",
          2433L
        )
      )

    # testing overall call
    testthat::expect_identical(
      object = using_function1,
      expected = results1
    )

    # testing t test value
    testthat::expect_identical(
      object = as.character(using_function1)[6],
      expected = as.character(results1)[6]
    )

    # testing cohens d size
    testthat::expect_identical(
      object = as.character(using_function1[14]),
      expected = as.character(results1[14])
    )

    # testing sample size
    testthat::expect_identical(
      object = using_function1[23],
      expected = results1[23]
    )
  }
)
