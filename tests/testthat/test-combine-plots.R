iris_species_plot <- function(species) {
  ggplot2::ggplot(
    data = subset(iris, iris$Species == species),
    aes(x = Sepal.Length, y = Sepal.Width)
  ) +
    geom_point() +
    labs(title = species)
}

iris_plotlist <- list(
  iris_species_plot("setosa"),
  iris_species_plot("versicolor")
)

iris_annotation_args <- list(
  tag_levels = "a",
  title = "Dataset: Iris Flower dataset",
  subtitle = "Edgar Anderson collected this data",
  caption = "Note: Only two species of flower are displayed"
)

test_that("checking if combining plots works", {
  set.seed(123)
  expect_doppelganger(
    title = "defaults work as expected",
    fig = combine_plots(
      plotlist = iris_plotlist,
      annotation.args = iris_annotation_args
    )
  )
})

test_that("annotation title stays bold when adding a custom annotation theme", {
  set.seed(123)
  expect_doppelganger(
    title = "annotation title remains bold with custom theme",
    fig = combine_plots(
      plotlist = iris_plotlist,
      annotation.args = c(
        iris_annotation_args,
        list(
          theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 18))
        )
      )
    )
  )
})
