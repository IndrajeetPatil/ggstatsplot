test_that(
  desc = "checking if combining plots works",
  code = {
    p1 <- ggplot2::ggplot(
      data = subset(iris, iris$Species == "setosa"),
      aes(x = Sepal.Length, y = Sepal.Width)
    ) +
      geom_point() +
      labs(title = "setosa")

    p2 <- ggplot2::ggplot(
      data = subset(iris, iris$Species == "versicolor"),
      aes(x = Sepal.Length, y = Sepal.Width)
    ) +
      geom_point() +
      labs(title = "versicolor")

    set.seed(123)
    expect_doppelganger(
      title = "defaults work as expected",
      fig = combine_plots(
        plotlist = list(p1, p2),
        annotation.args = list(
          tag_levels = "a",
          title = "Dataset: Iris Flower dataset",
          subtitle = "Edgar Anderson collected this data",
          caption = "Note: Only two species of flower are displayed"
        )
      )
    )
  }
)
