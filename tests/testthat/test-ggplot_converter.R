context("ggplot_converter")

# ggplot_converter works -------------------------------------------------

testthat::test_that(
  desc = "ggplot_converter work",
  code = {
    testthat::skip_on_cran()
    library(ggplot2)

    # creating objects that are not of ggplot type
    p1 <- ggExtra::ggMarginal(ggplot(mtcars, aes(mpg, wt)) + geom_point() +
      geom_smooth(method = "lm"))
    p2 <- NULL

    # testing conversion
    testthat::expect_identical(
      class(ggstatsplot::ggplot_converter(p1)),
      c("gg", "ggplot")
    )
    testthat::expect_identical(
      class(ggstatsplot::ggplot_converter(p2)),
      c("gg", "ggplot")
    )
  }
)
