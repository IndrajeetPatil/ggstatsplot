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

    # when the grouping and labelling variable are the same, expect error message
    testthat::expect_output(
      grouped_ggscatterstats(
        data = iris,
        x = Sepal.Length,
        y = Petal.Width,
        grouping.var = Species,
        label.var = Species
      ), "Error:"
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
        label.expression = "budget > 150",
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
        label.expression = "budget > 150",
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
    testthat::skip_on_cran()

    # should return a list of length 3
    ls_results <- ggstatsplot::grouped_ggscatterstats(
      data = iris,
      x = Sepal.Length,
      y = Petal.Width,
      grouping.var = Species,
      return = "subtitle",
      results.subtitle = NULL,
      messages = FALSE
    )

    # tests
    testthat::expect_equal(length(ls_results), 3L)
    testthat::expect_null(ls_results[[1]], NULL)
    testthat::expect_null(ls_results[[2]], NULL)
    testthat::expect_null(ls_results[[3]], NULL)
  }
)
