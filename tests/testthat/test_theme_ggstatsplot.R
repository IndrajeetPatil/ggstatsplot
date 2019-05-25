context("theme_ggstatsplot")

# `theme_ggstatsplot()` works --------------------------------------------------

testthat::test_that(
  desc = "`theme_ggstatsplot()` works",
  code = {
    testthat::skip_on_cran()

    set.seed(123)
    library(ggplot2)

    # plot
    p <-
      ggplot(msleep, aes(vore, brainwt)) + geom_point(na.rm = TRUE)

    # changing the basic plot
    p1 <- p +
      ggstatsplot::theme_ggstatsplot(ggstatsplot.layer = FALSE)
    p2 <- p +
      ggstatsplot::theme_ggstatsplot(ggplot2::theme_dark())
    p3 <- p +
      ggstatsplot::theme_ggstatsplot(ggplot2::theme_minimal())

    # check if outputs are ggplot objects
    testthat::expect_identical(class(p1), c("gg", "ggplot"))
    testthat::expect_identical(class(p2), c("gg", "ggplot"))
    testthat::expect_identical(class(p3), c("gg", "ggplot"))
  }
)
