context("line_labeller")

# line_labeller works ------------------------------------------------

testthat::test_that(
  desc = "line_labeller works",
  code = {
    testthat::skip_on_cran()

    set.seed(123)
    library(ggplot2)

    # plot
    p0 <-
      ggplot(msleep, aes(sleep_rem, brainwt)) + geom_point(na.rm = TRUE)

    # adding label
    # mean
    p <- ggstatsplot:::line_labeller(
      plot = p0,
      x = mean(msleep$sleep_rem, na.rm = TRUE),
      y = mean(msleep$brainwt, na.rm = TRUE),
      label.text = "mean",
      k = 3,
      line.direction = "vline",
      color = "blue",
      jitter = -0.15
    )

    # median
    p1 <- ggstatsplot:::line_labeller(
      plot = p0,
      x = mean(msleep$sleep_rem, na.rm = TRUE),
      y = mean(msleep$brainwt, na.rm = TRUE),
      label.text = "median",
      k = 4,
      line.direction = "hline",
      color = "green",
      jitter = 0.25
    )

    # build the plot
    pb <- ggplot2::ggplot_build(p)
    pb1 <- ggplot2::ggplot_build(p1)

    # testing layers
    testthat::expect_equal(dim(pb$data[[1]]), c(83L, 10L))
    testthat::expect_equal(
      class(pb$data[[2]]$label[[1]]),
      "call"
    )
    testthat::expect_equal(
      pb$data[[2]]$label[[1]],
      ggplot2::expr("mean" == "1.875")
    )
    testthat::expect_identical(pb$data[[2]]$colour[[1]], "blue")
    testthat::expect_equal(pb$data[[2]]$x[[1]], 1.87541, tolerance = 0.001)
    testthat::expect_equal(pb$data[[2]]$y[[1]], 0.2393442, tolerance = 0.001)

    testthat::expect_equal(
      pb1$data[[2]]$label[[1]],
      ggplot2::expr("median" == "0.2816")
    )
    testthat::expect_identical(pb1$data[[2]]$colour[[1]], "green")
    testthat::expect_equal(pb1$data[[2]]$x[[1]], 2.344262, tolerance = 0.001)
    testthat::expect_equal(pb1$data[[2]]$y[[1]], 0.2815814, tolerance = 0.001)
  }
)
