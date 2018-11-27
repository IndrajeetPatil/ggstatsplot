# context ------------------------------------------------------------
context(desc = "ggbetweenstats")

# outlier labeling works -------------------------------------------------------

suppressPackageStartupMessages(library(rlang))

testthat::test_that(
  desc = "error when x and outlier.label are same",
  code = {
    testthat::expect_error(
      suppressWarnings(ggstatsplot::ggbetweenstats(
        data = iris,
        x = Species,
        y = Sepal.Length,
        outlier.label = Species
      ))
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

# checking labels and data from plot -------------------------------------

testthat::test_that(
  desc = "checking labels and data from plot",
  code = {
    set.seed(123)
    # creating the plot
    p <- ggstatsplot::ggbetweenstats(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      outlier.tagging = TRUE,
      outlier.label = name,
      conf.level = 0.99,
      bf.message = TRUE,
      messages = FALSE
    )

    # checking dimensions of data
    data_dims <- ggplot2::layer_data(p) %>%
      tibble::as_tibble(x = .) %>%
      dim(.)

    testthat::expect_equal(data_dims[1], 51L)
    testthat::expect_equal(data_dims[2], 13L)


    # checking displayed outlier labels
    outlier.labels <- ggplot2::layer_grob(p, i = 5L)$`1`$lab

    testthat::expect_equal(length(outlier.labels), 7L)
    testthat::expect_identical(outlier.labels[1], "Asian elephant")
    testthat::expect_identical(outlier.labels[7], "Giant armadillo")

    # range of data
    y_range <- ggplot2::layer_scales(p)$y$range$range

    testthat::expect_equal(y_range[1], -0.09710299, tolerance = 1e-5)
    testthat::expect_equal(y_range[2], 5.71200000, tolerance = 1e-5)

    # limits of data
    y_limits <- ggplot2::layer_scales(p)$y$limits

    testthat::expect_equal(y_limits[1], 0.00014, tolerance = 1e-3)
    testthat::expect_equal(y_limits[2], 5.71200, tolerance = 1e-3)

    # checking x-axis sample size labels
    x_labels <- ggplot2::layer_scales(p)$x$labels

    testthat::expect_identical(x_labels[1], "carni\n(n = 9)")
    testthat::expect_identical(x_labels[2], "herbi\n(n = 20)")
    testthat::expect_identical(x_labels[3], "insecti\n(n = 5)")
    testthat::expect_identical(x_labels[4], "omni\n(n = 17)")
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
