# context ------------------------------------------------------------
context(desc = "ggbetweenstats")

# outlier labeling works -------------------------------------------------------

testthat::test_that(
  desc = "error when x and outlier.label are same",
  code = {
    testthat::expect_error(
      object = ggstatsplot::ggbetweenstats(
        data = iris,
        x = Species,
        y = Sepal.Length,
        outlier.label = Species
      )
    )
  }
)

testthat::test_that(
  desc = "outlier.labeling works across vector types",
  code = {

    # `outlier.label` is numeric
    set.seed(123)
    testthat::expect_true(object = inherits(
      x = ggstatsplot::ggbetweenstats(
        data = dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.25),
        x = genre,
        y = rating,
        messages = FALSE,
        outlier.tagging = TRUE,
        outlier.label = length
      ),
      what = "gg"
    ))

    # `outlier.label` is factor
    set.seed(123)
    testthat::expect_true(object = inherits(
      x = ggstatsplot::ggbetweenstats(
        data = dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.25),
        x = genre,
        y = rating,
        messages = FALSE,
        outlier.tagging = TRUE,
        outlier.label = "title"
      ),
      what = "gg"
    ))


    # `outlier.label` is character
    # also x, y, and outlier.label arguments as characters
    set.seed(123)
    movies_long1 <-
      dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.25)
    movies_long1$title <- as.character(movies_long1$title)

    testthat::expect_true(object = inherits(
      x =
        ggstatsplot::ggbetweenstats(
          data = movies_long1,
          x = "genre",
          y = "rating",
          messages = FALSE,
          outlier.tagging = TRUE,
          outlier.label = "title",
          outlier.coef = 5
        ),
      what = "gg"
    ))
  }
)


# visual tests ------------------------------------------------------------

# haven't yet figured out how to implement tests using `vdiffr` package

# testthat::test_that(
#   desc = "ggbetweenstats works",
#   code = {
#     # plot to compare to
#     ggbetweenstats_anova <- ggstatsplot::ggbetweenstats(
#       data = datasets::iris,
#       x = Species,
#       y = Sepal.Length,
#       messages = FALSE
#     )
#     # comparison using vdiffr package
#        vdiffr::expect_doppelganger(
#          title = "ggbetweenstats_anova",
#          fig = ggbetweenstats_anova
#        )
#   }
# )
