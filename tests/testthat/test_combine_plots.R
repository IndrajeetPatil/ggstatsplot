context("combine_plots")

testthat::test_that(
  desc = "checking if combining plots works",
  code = {

    # plots
    set.seed(123)
    library(ggplot2)

    # creating basic plots
    p1 <- ggplot(aes(x = as.factor(am), y = wt), data = mtcars) + geom_point()
    p2 <- ggplot(aes(x = Species, y = Sepal.Length), data = iris) + geom_point()

    # combined plot
    p <- ggstatsplot::combine_plots(p1, p2,
      title.text = "combined plot",
      title.color = "blue",
      sub.text = "additional text",
      sub.color = "darkgreen",
      caption.text = "combined caption",
      caption.color = "red",
      labels = c("(a)", "(b)")
    )


    pb <- ggplot2::ggplot_build(p)

    # testing labels
    testthat::expect_identical(
      p$layers[[1]]$geom_params$grob$grobs[[19]]$label,
      "additional text"
    )
    testthat::expect_identical(
      class(p$layers[[1]]$geom_params$grob)[[1]],
      "gtable"
    )
  }
)
