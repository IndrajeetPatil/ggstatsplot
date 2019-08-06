context("grouped_ggscatterstats")

testthat::test_that(
  desc = "grouped_ggscatterstats works",
  code = {
    testthat::skip_on_cran()

    # expect error if no grouping variable is specified
    testthat::expect_error(
      grouped_ggscatterstats(
        data = iris,
        x = Sepal.Length,
        y = Petal.Width
      )
    )

    # without any labelling
    set.seed(123)
    testthat::expect_true(inherits(
      ggstatsplot::grouped_ggscatterstats(
        data = iris,
        x = Sepal.Length,
        y = Petal.Width,
        grouping.var = Species,
        results.subtitle = FALSE,
        marginal = FALSE,
        messages = TRUE
      ),
      what = "gg"
    ))

    # create a smaller dataset
    set.seed(123)
    df <- dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.25) %>%
      dplyr::filter(
        .data = ., mpaa %in% c("R", "PG-13"),
        genre %in% c("Drama", "Comedy")
      )

    # both quoted
    set.seed(123)
    testthat::expect_true(inherits(
      ggstatsplot::grouped_ggscatterstats(
        data = df,
        x = length,
        y = rating,
        label.expression = "length > 150",
        label.var = "title",
        grouping.var = mpaa,
        type = "bf",
        marginal = FALSE,
        messages = TRUE
      ),
      what = "gg"
    ))

    # both unquoted
    set.seed(123)
    testthat::expect_true(inherits(
      ggstatsplot::grouped_ggscatterstats(
        data = df,
        x = length,
        y = rating,
        label.expression = budget > 150,
        label.var = title,
        grouping.var = mpaa,
        results.subtitle = FALSE,
        marginal = FALSE,
        messages = FALSE
      ),
      what = "gg"
    ))

    # one quoted, one unquoted
    set.seed(123)
    testthat::expect_true(inherits(
      ggstatsplot::grouped_ggscatterstats(
        data = df,
        x = length,
        y = rating,
        label.expression = budget > 150,
        label.var = "title",
        grouping.var = mpaa,
        type = "p",
        marginal = FALSE,
        messages = FALSE
      ),
      what = "gg"
    ))

    set.seed(123)
    testthat::expect_true(inherits(
      ggstatsplot::grouped_ggscatterstats(
        data = df,
        x = "length",
        y = "rating",
        grouping.var = "mpaa",
        type = "r",
        label.expression = "budget > 150",
        label.var = title,
        results.subtitle = FALSE,
        messages = FALSE,
        marginal = FALSE
      ),
      what = "gg"
    ))

    # without point labelling
    set.seed(123)
    testthat::expect_true(inherits(
      ggstatsplot::grouped_ggscatterstats(
        data = df,
        x = "length",
        y = rating,
        grouping.var = mpaa,
        label.expression = "length > 150",
        type = "np",
        results.subtitle = FALSE,
        messages = FALSE,
        marginal = FALSE
      ),
      what = "gg"
    ))

    # labeling all points (without expression, i.e.)
    set.seed(123)
    testthat::expect_true(inherits(
      ggstatsplot::grouped_ggscatterstats(
        data = dplyr::sample_frac(tbl = df, size = 0.1),
        x = "length",
        y = rating,
        grouping.var = mpaa,
        label.var = title,
        label.expression = NULL,
        type = "np",
        results.subtitle = FALSE,
        messages = FALSE,
        marginal = FALSE
      ),
      what = "gg"
    ))

    # checking if ggplot component addition works
    set.seed(123)
    testthat::expect_true(inherits(
      ggstatsplot::grouped_ggscatterstats(
        data = ggplot2::msleep,
        x = sleep_total,
        y = bodywt,
        marginal = FALSE,
        results.subtitle = FALSE,
        grouping.var = "vore",
        xlab = "total sleep",
        ylab = "body weight",
        title = "mammalian sleep dataset",
        caption = "source: ggplot2 package",
        type = "bf",
        ggplot.component = scale_y_continuous(breaks = seq(0, 6000, 1000)),
        messages = FALSE
      ),
      what = "gg"
    ))
  }
)

# subtitle return --------------------------------------------------

testthat::test_that(
  desc = "subtitle return",
  code = {
    # should return a list of length 3
    set.seed(123)
    ls_results <- ggstatsplot::grouped_ggscatterstats(
      data = dplyr::filter(
        .data = ggstatsplot::movies_long,
        genre %in% c("Action", "Action Comedy", "Action Drama", "Comedy")
      ),
      x = rating,
      y = length,
      k = 3,
      conf.level = 0.99,
      grouping.var = genre,
      return = "subtitle",
      messages = FALSE
    )

    # tests
    testthat::expect_equal(length(ls_results), 4L)
    testthat::expect_identical(
      ls_results[[1]],
      ggplot2::expr(
        paste(
          NULL,
          italic("t"),
          "(",
          "184",
          ") = ",
          "10.145",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("r")["Pearson"],
          " = ",
          "0.599",
          ", CI"["99%"],
          " [",
          "0.463",
          ", ",
          "0.707",
          "]",
          ", ",
          italic("n")["pairs"],
          " = ",
          186L
        )
      )
    )
    testthat::expect_identical(
      ls_results[[2]],
      ggplot2::expr(
        paste(
          NULL,
          italic("t"),
          "(",
          "86",
          ") = ",
          "3.626",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("r")["Pearson"],
          " = ",
          "0.364",
          ", CI"["99%"],
          " [",
          "0.102",
          ", ",
          "0.579",
          "]",
          ", ",
          italic("n")["pairs"],
          " = ",
          88L
        )
      )
    )
    testthat::expect_identical(
      ls_results[[3]],
      ggplot2::expr(
        paste(
          NULL,
          italic("t"),
          "(",
          "120",
          ") = ",
          "7.173",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("r")["Pearson"],
          " = ",
          "0.548",
          ", CI"["99%"],
          " [",
          "0.362",
          ", ",
          "0.692",
          "]",
          ", ",
          italic("n")["pairs"],
          " = ",
          122L
        )
      )
    )
    testthat::expect_identical(
      ls_results[[4]],
      ggplot2::expr(
        paste(
          NULL,
          italic("t"),
          "(",
          "258",
          ") = ",
          "5.202",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("r")["Pearson"],
          " = ",
          "0.308",
          ", CI"["99%"],
          " [",
          "0.156",
          ", ",
          "0.446",
          "]",
          ", ",
          italic("n")["pairs"],
          " = ",
          260L
        )
      )
    )
  }
)
