context("grouped_ggscatterstats")

testthat::test_that(
  desc = "grouped_ggscatterstats works",
  code = {
    testthat::skip_on_cran()

    # when the grouping and labelling variable are the same, the function
    # shouldn't work
    testthat::expect_output(
      grouped_ggscatterstats(
        data = iris,
        x = Sepal.Length,
        y = Petal.Width,
        grouping.var = Species,
        label.var = Species
      ), "Error:"
    )

    # both quoted
    set.seed(123)
    testthat::expect_true(inherits(
      ggstatsplot::grouped_ggscatterstats(
        data = dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.25) %>%
          dplyr::filter(
            .data = ., mpaa %in% c("R", "PG-13"),
            genre %in% c("Drama", "Comedy")
          ),
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
        data = dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.25) %>%
          dplyr::filter(
            .data = ., mpaa %in% c("R", "PG-13"),
            genre %in% c("Drama", "Comedy")
          ),
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
        data = dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.25) %>%
          dplyr::filter(
            .data = ., mpaa %in% c("R", "PG-13"),
            genre %in% c("Drama", "Comedy")
          ),
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
        data = dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.25) %>%
          dplyr::filter(
            .data = ., mpaa %in% c("R", "PG-13"),
            genre %in% c("Drama", "Comedy")
          ),
        x = "length",
        y = "rating",
        grouping.var = "mpaa",
        type = "r",
        label.expression = "budget > 150",
        label.var = title,
        messages = FALSE,
        marginal = FALSE
      ),
      what = "gg"
    ))

    # without point labelling
    set.seed(123)
    testthat::expect_true(inherits(
      ggstatsplot::grouped_ggscatterstats(
        data = dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.25) %>%
          dplyr::filter(
            .data = ., mpaa %in% c("R", "PG-13"),
            genre %in% c("Drama", "Comedy")
          ),
        x = "length",
        y = rating,
        grouping.var = mpaa,
        type = "np",
        messages = FALSE,
        marginal = FALSE
      ),
      what = "gg"
    ))
  }
)
