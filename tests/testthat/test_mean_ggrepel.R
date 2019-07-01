context(desc = "mean_ggrepel")

# mean_ggrepel works ----------------------------------------------

testthat::test_that(
  desc = "mean_ggrepel works",
  code = {
    testthat::skip_on_cran()

    set.seed(123)
    library(ggplot2)

    # make a plot
    p <- ggplot(data = msleep, aes(x = vore, y = brainwt)) +
      geom_boxplot(na.rm = TRUE)

    p_dat <- ggstatsplot:::mean_labeller(
      data = msleep,
      x = vore,
      y = brainwt,
      mean.ci = TRUE
    ) %>%
      dplyr::rename(.data = ., x = vore, y = brainwt)

    # add means
    p_mean <- ggstatsplot:::mean_ggrepel(
      plot = p,
      mean.data = p_dat,
      mean.color = "blue"
    )

    p_new <- ggstatsplot:::mean_ggrepel(
      plot = p,
      mean.data = p_dat,
      mean.color = "blue",
      inherit.aes = FALSE
    )

    # build plot for tests
    pb_mean <- ggplot2::ggplot_build(p_mean)

    # check data
    testthat::expect_equal(dim(pb_mean$data[[1]]), c(5L, 25L))
    testthat::expect_equal(dim(pb_mean$data[[2]]), c(5L, 12L))
    testthat::expect_equal(dim(pb_mean$data[[3]]), c(4L, 15L))
    testthat::expect_equal(
      pb_mean$data[[2]]$y,
      c(0.07925556, 0.62159750, 0.02155000, 0.14573118, 0.00762600),
      tolerance = 0.001
    )
    testthat::expect_identical(
      as.character(unique(pb_mean$data[[2]]$colour)),
      "blue"
    )

    testthat::expect_is(p_new, "ggplot")
  }
)
