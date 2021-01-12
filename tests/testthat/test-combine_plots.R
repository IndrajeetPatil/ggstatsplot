# combine_plots --------------------------------------------------------

test_that(
  desc = "checking if combining plots works",
  code = {
    skip_on_cran()

    # setup
    set.seed(123)
    library(ggplot2)

    # creating basic plots
    p1 <- ggplot(aes(x = as.factor(am), y = wt), data = mtcars) +
      geom_point()
    p2 <- ggplot(aes(x = Species, y = Sepal.Length), data = iris) +
      geom_point()

    # combined plot with everything
    p <-
      ggstatsplot::combine_plots(p1, p2,
        title.text = "combined plot",
        title.color = "blue",
        sub.text = "additional text",
        sub.color = "darkgreen",
        caption.text = "combined caption",
        caption.color = "red",
        labels = c("(a)", "(b)")
      )

    # only title
    p1 <- ggstatsplot::combine_plots(p1, p2,
      title.text = "combined plot"
    )

    # only caption
    p2 <- ggstatsplot::combine_plots(p1, p2,
      caption.text = "combined caption"
    )

    # built plot
    pb <- ggplot2::ggplot_build(p)

    # testing labels
    expect_identical(
      p$layers[[1]]$geom_params$grob$grobs[[19]]$label,
      "additional text"
    )
    expect_identical(
      class(p$layers[[1]]$geom_params$grob),
      c("gtable", "gTree", "grob", "gDesc")
    )
    expect_identical(class(p), c("gg", "ggplot"))
    expect_identical(class(p1), c("gg", "ggplot"))
    expect_identical(class(p2), c("gg", "ggplot"))
  }
)


# combine_plots2 --------------------------------------------------------

test_that(
  desc = "checking if combining plots works with the other version",
  code = {
    skip_on_cran()

    # setup
    set.seed(123)
    library(ggplot2)

    # creating basic plots
    p1 <- ggplot(aes(x = as.factor(am), y = wt), data = mtcars) +
      geom_point()
    p2 <- ggplot(aes(x = Species, y = Sepal.Length), data = iris) +
      geom_point()

    # combined plot with everything
    p <-
      ggstatsplot::combine_plots2(
        plotlist = list(p1, p2),
        title.text = "combined plot",
        title.color = "blue",
        sub.text = "additional text",
        sub.color = "darkgreen",
        caption.text = "combined caption",
        caption.color = "red",
        labels = c("(a)", "(b)")
      )

    # only title
    p1 <- ggstatsplot::combine_plots2(
      plotlist = list(p1, p2),
      title.text = "combined plot"
    )

    # only caption
    p2 <- ggstatsplot::combine_plots2(
      plotlist = list(p1, p2),
      caption.text = "combined caption"
    )

    # built plot
    pb <- ggplot2::ggplot_build(p)

    # testing labels
    expect_identical(
      p$layers[[1]]$geom_params$grob$grobs[[19]]$label,
      "additional text"
    )
    expect_identical(
      class(p$layers[[1]]$geom_params$grob),
      c("gtable", "gTree", "grob", "gDesc")
    )
    expect_identical(class(p), c("gg", "ggplot"))
    expect_identical(class(p1), c("gg", "ggplot"))
    expect_identical(class(p2), c("gg", "ggplot"))
  }
)
