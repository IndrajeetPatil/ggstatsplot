context("grouped_ggscatterstats")

testthat::test_that(
  desc = "grouped_ggscatterstats works",
  code = {
    skip_on_cran()

    # when the grouping and labelling variable are the same, the function
    # shouldn't work
    testthat::expect_error(
      grouped_ggscatterstats(
        data = iris,
        x = Sepal.Length,
        y = Petal.Width,
        grouping.var = Species,
        label.var = Species
      )
    )

    # when arguments are entered as bare expressions
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
        type = "p",
        marginal = FALSE,
        messages = TRUE
      ),
      what = "gg"
    ))

    # when arguments are entered as character
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
        messages = FALSE,
        marginal = FALSE
      ),
      what = "gg"
    ))
  }
)
