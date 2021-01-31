# combine_plots --------------------------------------------------------

test_that(
  desc = "checking if combining plots works",
  code = {
    skip_on_cran()

    # setup
    set.seed(123)
    library(ggplot2)

    # preparing the first plot
    p1 <-
      ggplot2::ggplot(
        data = subset(iris, iris$Species == "setosa"),
        aes(x = Sepal.Length, y = Sepal.Width)
      ) +
      geom_point() +
      labs(title = "setosa")

    # preparing the second plot
    p2 <-
      ggplot2::ggplot(
        data = subset(iris, iris$Species == "versicolor"),
        aes(x = Sepal.Length, y = Sepal.Width)
      ) +
      geom_point() +
      labs(title = "versicolor")

    # combining the plot with a title and a caption
    p <-
      combine_plots(
        plotlist = list(p1, p2),
        annotation.args = list(
          tag_levels = "a",
          title = "Dataset: Iris Flower dataset",
          subtitle = "Edgar Anderson collected this data",
          caption = "Note: Only two species of flower are displayed"
        )
      )

    # built plot
    pb <- ggplot2::ggplot_build(p)

    # testing labels
    expect_equal(
      pb$plot$labels,
      list(title = "versicolor", x = "Sepal.Length", y = "Sepal.Width")
    )

    expect_equal(
      pb$plot$patches$annotation,
      structure(
        list(
          title = "Dataset: Iris Flower dataset",
          subtitle = "Edgar Anderson collected this data",
          caption = "Note: Only two species of flower are displayed",
          tag_levels = "a",
          tag_prefix = NULL,
          tag_suffix = NULL,
          tag_sep = NULL,
          theme = structure(
            list(),
            .Names = character(0),
            class = c(
              "theme",
              "gg"
            ),
            complete = FALSE,
            validate = TRUE
          )
        ),
        class = "plot_annotation"
      )
    )
    expect_identical(class(p), c("patchwork", "gg", "ggplot"))
  }
)
