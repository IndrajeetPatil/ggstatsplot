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
  }
)
