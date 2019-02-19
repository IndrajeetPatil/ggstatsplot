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

    # add means
    p_mean <- ggstatsplot:::mean_ggrepel(
      plot = p,
      data = msleep,
      x = vore,
      y = brainwt,
      mean.ci = TRUE,
      mean.color = "blue"
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
  }
)
