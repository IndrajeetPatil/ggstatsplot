# shared fixtures for test-combine-plots.R

iris_species_plot <- function(species) {
  ggplot2::ggplot(
    data = subset(datasets::iris, Species == species),
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
