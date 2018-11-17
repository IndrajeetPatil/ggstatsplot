# context ------------------------------------------------------------
context(desc = "grouped_ggbetweenstats")

# outlier labeling works -----------------------------------------------------

testthat::test_that(
  desc = "grouping.var works across vector types",
  code = {

    # `outlier.label` is numeric
    set.seed(123)
    testthat::expect_true(object = inherits(
      x = ggstatsplot::grouped_ggbetweenstats(
        data = dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.25) %>%
          dplyr::filter(
            .data = ., mpaa %in% c("R", "PG-13"),
            genre %in% c("Drama", "Comedy")
          ),
        x = genre,
        y = rating,
        grouping.var = mpaa,
        type = "p",
        plot.type = "box",
        bf.message = TRUE,
        pairwise.comparisons = TRUE,
        pairwise.annotation = "p.value",
        messages = FALSE,
        outlier.tagging = TRUE,
        outlier.label = length
      ),
      what = "gg"
    ))

    # `outlier.label` is factor
    set.seed(123)
    testthat::expect_true(object = inherits(
      x = ggstatsplot::grouped_ggbetweenstats(
        data = dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.25) %>%
          dplyr::filter(
            .data = ., mpaa %in% c("R", "PG-13"),
            genre %in% c("Drama", "Comedy")
          ),
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
    movies_long1 <-
      dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.25) %>%
      dplyr::filter(
        .data = ., mpaa %in% c("R", "PG-13"),
        genre %in% c("Drama", "Comedy")
      )
    movies_long1$title <- as.character(movies_long1$title)

    testthat::expect_true(object = inherits(
      x =
        ggstatsplot::grouped_ggbetweenstats(
          data = movies_long1,
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
