context("subtitle_t_onesample")

# subtitle ---------------------------------------------------------

testthat::test_that(
  desc = "subtitle_t_onesample works",
  code = {

    # parametric

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
          ", CI"["95%"],
          " [",
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
  }
)

# message checks ---------------------------------------------------------

testthat::test_that(
  desc = "subtitle_t_onesample works",
  code = {

    # message
    set.seed(123)
    p_message1 <- capture.output(
      ggstatsplot::subtitle_t_onesample(
        x = ToothGrowth$len,
        nboot = 10,
        type = "r",
        robust.estimator = "median"
      )
    )

    p_message2 <- capture.output(
      ggstatsplot::subtitle_t_onesample(
        x = ToothGrowth$len,
        nboot = 25,
        type = "r",
        robust.estimator = "mom"
      )
    )

    p_message3 <- capture.output(
      ggstatsplot::subtitle_t_onesample(
        x = ToothGrowth$len,
        conf.level = 0.95,
        nboot = 20,
        type = "r",
        robust.estimator = "onestep"
      )
    )

    # checking captured messages
    testthat::expect_match(p_message1[1],
      "median computed with 10",
      fixed = TRUE
    )

    testthat::expect_match(p_message2[1],
      "mom computed with 25",
      fixed = TRUE
    )

    testthat::expect_match(p_message3[1],
      "onestep computed with 20",
      fixed = TRUE
    )
  }
)
