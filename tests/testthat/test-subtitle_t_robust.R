context("subtitle_t_robust")

# within-subjects ------------------------------------------------------------

testthat::test_that(
  desc = "subtitle_t_robust - within-subjects - without NAs",
  code = {
    # subtitle
    set.seed(123)
    using_function1 <- subtitle_t_robust(
      data = dplyr::filter(
        ggstatsplot::iris_long,
        condition %in% c("Sepal.Length", "Sepal.Width")
      ),
      x = "condition",
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
        italic("n")["pairs"],
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
        data = dplyr::filter(
          ggstatsplot::iris_long,
          condition %in% c("Sepal.Length", "Sepal.Width")
        ),
        x = condition,
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

testthat::test_that(
  desc = "subtitle_t_robust - within-subjects - with NAs",
  code = {
    # subtitle
    set.seed(123)
    using_function1 <- subtitle_t_robust(
      data = dplyr::filter(ggstatsplot::bugs_long, condition %in% c("HDHF", "HDLF")),
      x = "condition",
      y = desire,
      paired = TRUE,
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
        "71",
        ") = ",
        "3.274",
        ", ",
        italic("p"),
        " = ",
        "0.002",
        ", ",
        italic(xi),
        " = ",
        "0.273",
        ", CI"["99%"],
        " [",
        "0.066",
        ", ",
        "0.490",
        "]",
        ", ",
        italic("n")["pairs"],
        " = ",
        90L
      )
    )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)


# between-subjects ------------------------------------------------------------

testthat::test_that(
  desc = "subtitle_t_robust - between-subjects - without NAs",
  code = {
    testthat::skip_on_cran()

    # subtitle
    set.seed(123)
    using_function1 <- subtitle_t_robust(
      data = mtcars,
      x = am,
      y = "wt",
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
        italic("n")["obs"],
        " = ",
        32L
      )
    )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)
