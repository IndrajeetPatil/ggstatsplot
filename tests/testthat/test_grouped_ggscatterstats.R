context("grouped_ggscatterstats")

testthat::test_that(
  desc = "grouped_ggscatterstats works",
  code = {

    # when the grouping and labelling variable are the same, the function
    # shouldn't work
    testthat::expect_error(
      object = grouped_ggscatterstats(
        data = iris,
        x = Sepal.Length,
        y = Petal.Width,
        grouping.var = Species,
        label.var = Species
      )
    )

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(object = inherits(
      x = ggstatsplot::grouped_ggscatterstats(
        data = dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.25) %>%
          dplyr::filter(
            .data = ., mpaa %in% c("R", "PG-13"),
            genre %in% c("Drama", "Comedy")
          ),
        x = length,
        y = rating,
        grouping.var = mpaa,
        marginal = FALSE,
        messages = FALSE
      ),
      what = "gg"
    ))

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(object = inherits(
      x = ggstatsplot::grouped_ggscatterstats(
        data = dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.25) %>%
          dplyr::filter(
            .data = ., mpaa %in% c("R", "PG-13"),
            genre %in% c("Drama", "Comedy")
          ),
        x = "length",
        y = "rating",
        grouping.var = "mpaa",
        messages = FALSE,
        marginal = FALSE
      ),
      what = "gg"
    ))
  }
)
