# context -----------------------------------------------------------------
context(desc = "grouped_ggbetweenstats")

# outlier labeling works --------------------------------------------------

testthat::test_that(
  desc = "grouping.var works across vector types",
  code = {
    testthat::skip_on_cran()

    # creating a smaller dataframe
    set.seed(123)
    dat <- dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.25) %>%
      dplyr::filter(
        .data = ., mpaa %in% c("R", "PG-13"),
        genre %in% c("Drama", "Comedy")
      )

    # expect error when no grouping.var is specified
    testthat::expect_error(
      ggstatsplot::grouped_ggbetweenstats(
        data = dat,
        x = genre,
        y = rating
      )
    )

    # expect error when x and grouping.var are same
    testthat::expect_output(
      ggstatsplot::grouped_ggbetweenstats(
        data = dat,
        x = genre,
        y = rating,
        grouping.var = genre
      )
    )

    # outlier tagging is not required
    ggstatsplot::grouped_ggbetweenstats(
      data = dat,
      x = genre,
      y = rating,
      grouping.var = mpaa,
      outlier.tagging = FALSE,
      messages = TRUE
    )

    # `outlier.label` is not specified
    set.seed(123)
    testthat::expect_true(inherits(
      ggstatsplot::grouped_ggbetweenstats(
        data = dat,
        x = genre,
        y = rating,
        grouping.var = mpaa,
        type = "p",
        plot.type = "box",
        bf.message = TRUE,
        outlier.tagging = TRUE,
        pairwise.comparisons = TRUE,
        pairwise.annotation = "p.value",
        messages = TRUE
      ),
      what = "gg"
    ))

    # `outlier.label` is factor
    set.seed(123)
    testthat::expect_true(inherits(
      ggstatsplot::grouped_ggbetweenstats(
        data = dat,
        x = genre,
        y = rating,
        grouping.var = "mpaa",
        type = "np",
        plot.type = "violin",
        pairwise.comparisons = TRUE,
        messages = FALSE,
        outlier.tagging = TRUE,
        outlier.label = title
      ),
      what = "gg"
    ))


    # `outlier.label` is character
    # also x, y, and outlier.label arguments as characters
    set.seed(123)
    dat$title <- as.character(dat$title)

    testthat::expect_true(inherits(
      ggstatsplot::grouped_ggbetweenstats(
        data = dat,
        x = "genre",
        y = "rating",
        grouping.var = mpaa,
        messages = FALSE,
        type = "r",
        pairwise.comparisons = TRUE,
        outlier.tagging = TRUE,
        outlier.label = "title",
        outlier.coef = 5
      ),
      what = "gg"
    ))
  }
)

# outlier labeling works --------------------------------------------------

testthat::test_that(
  desc = "grouping.var works across vector types",
  code = {
    testthat::skip_on_cran()

    testthat::expect_true(inherits(
      ggstatsplot::grouped_ggbetweenstats(
        data = dplyr::filter(
          ggstatsplot::movies_long,
          genre %in% c("Action", "Comedy"),
          mpaa %in% c("R", "PG")
        ),
        x = genre,
        y = rating,
        grouping.var = mpaa,
        results.subtitle = FALSE,
        ggplot.component = ggplot2::scale_y_continuous(breaks = seq(1, 9, 1)),
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
    ls_results <- ggstatsplot::grouped_ggbetweenstats(
      data = iris_long,
      x = condition,
      y = value,
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
