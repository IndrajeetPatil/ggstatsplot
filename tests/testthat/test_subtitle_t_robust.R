context("subtitle_t_robust")

# within-subjects ------------------------------------------------------------

testthat::test_that(
  desc = "subtitle_t_robust - within-subjects",
  code = {
    testthat::skip_on_cran()

    # creating a dataframe
    df <- iris %>%
      tidyr::gather(key, value, Sepal.Length, Sepal.Width) %>%
      tibble::as_tibble(x = .)

    # subtitle
    set.seed(123)
    using_function1 <- subtitle_t_robust(
      data = df,
      x = key,
      y = value,
      paired = TRUE,
      conf.level = 0.90,
      k = 4,
      messages = FALSE
    )

    # expected
    results1 <- ggplot2::expr(
      paste(
        NULL,
        italic("t"),
        "(",
        "119",
        ") = ",
        "31.9809",
        ", ",
        italic("p"),
        " = ",
        "< 0.001",
        ", ",
        italic(xi),
        " = ",
        "0.9265",
        ", CI"["90%"],
        " [",
        "0.9193",
        ", ",
        "0.9367",
        "]",
        ", ",
        italic("n"),
        " = ",
        150L
      )
    )

    # testing overall call
    testthat::expect_identical(using_function1, results1)

    # checking if messages are okay
    set.seed(123)
    p_message <- capture.output(
      ggstatsplot::subtitle_t_robust(
        data = df,
        x = key,
        y = value,
        paired = TRUE,
        conf.level = 0.99,
        nboot = 20,
        k = 3,
        messages = TRUE
      )
    )

    # checking captured messages
    testthat::expect_match(p_message[1],
      "99% CI for effect size estimate was computed with 20",
      fixed = TRUE
    )
  }
)


# between-subjects ------------------------------------------------------------

testthat::test_that(
  desc = "subtitle_t_robust - between-subjects",
  code = {
    testthat::skip_on_cran()

    # subtitle
    set.seed(123)
    using_function1 <- subtitle_t_robust(
      data = mtcars,
      x = am,
      y = wt,
      paired = FALSE,
      conf.level = 0.99,
      k = 3,
      messages = FALSE
    )

    # expected
    results1 <- ggplot2::expr(
      paste(
        NULL,
        italic("t"),
        "(",
        "24.816",
        ") = ",
        "5.255",
        ", ",
        italic("p"),
        " = ",
        "< 0.001",
        ", ",
        italic(xi),
        " = ",
        "0.812",
        ", CI"["99%"],
        " [",
        "0.699",
        ", ",
        "0.986",
        "]",
        ", ",
        italic("n"),
        " = ",
        32L
      )
    )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)
