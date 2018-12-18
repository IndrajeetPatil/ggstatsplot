context("grouped_ggscatterstats")

testthat::test_that(
  desc = "grouped_ggscatterstats works",
  code = {

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
        label.expression = budget > 150,
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
        label.expression = "budget > 150",
        label.var = title,
        messages = FALSE,
        marginal = FALSE
      ),
      what = "gg"
    ))
  }
)
