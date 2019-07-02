context("histo_labeller")

# computing y coordinates ------------------------------------------------

testthat::test_that(
  desc = "y coordinate for labeller works",
  code = {
    set.seed(123)
    library(ggplot2)

    # plot
    p <- ggplot(mtcars, aes(wt, mpg)) + geom_point(na.rm = TRUE)

    # y coordinates
    y_label_pos <- median(
      x = ggplot2::layer_scales(p)$y$range$range,
      na.rm = TRUE
    )

    testthat::expect_equal(y_label_pos, 22.15, tolerance = 0.01)
  }
)

# checking if labeling works ------------------------------------------------

testthat::test_that(
  desc = "checking if labeling works",
  code = {
    testthat::skip_on_cran()

    set.seed(123)
    library(ggplot2)

    # plot
    plot <-
      ggplot(dplyr::starwars, aes(height, mass)) +
      geom_point(na.rm = TRUE)

    # y coordinates
    y_label_pos <- median(
      x = ggplot2::layer_scales(plot)$y$range$range,
      na.rm = TRUE
    )

    testthat::expect_equal(y_label_pos, 686.5, tolerance = 0.01)

    # adding labels
    p <- ggstatsplot:::histo_labeller(
      plot = plot,
      x = dplyr::starwars$height,
      y.label.position = y_label_pos,
      test.value.line = TRUE,
      test.value = 160,
      test.value.color = "green",
      centrality.color = "red"
    )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # checking dimensions of data
    data_dims <- ggplot2::layer_data(p) %>%
      tibble::as_tibble(x = .) %>%
      dim(.)

    testthat::expect_equal(data_dims[1], 87L)
    testthat::expect_equal(data_dims[2], 10L)

    # checking aesthetic parameters
    testthat::expect_identical(p$layers[[2]]$aes_params$linetype, "dashed")
    testthat::expect_identical(p$layers[[2]]$aes_params$colour, "green")
    testthat::expect_identical(p$layers[[4]]$aes_params$linetype, "dashed")
    testthat::expect_identical(p$layers[[4]]$aes_params$colour, "red")

    testthat::expect_identical(
      unique(pb$data[[5]]$label)[[1]],
      ggplot2::expr("mean" == "174.36")
    )
    testthat::expect_identical(
      unique(pb$data[[3]]$label)[[1]],
      ggplot2::expr("test" == "160")
    )
  }
)
