library(ggplot2)
library(zeallot)
library(dplyr)
library(statsExpressions)
library(rlang)
library(correlation)
library(datawizard)
library(ggcorrplot)
library(ggrepel)
library(ggside)
library(ggsignif)
library(glue)
library(insight)
library(paletteer)
library(parameters)
library(patchwork)
library(performance)
library(purrr)
library(tidyr)
library(utils)


fichiers_R <- list.files(path = "R", pattern = "\\.R$", full.names = TRUE)
for (fichier in fichiers_R) {
  message("Exécution de : ", fichier)
  source(fichier, echo = TRUE)  # echo = TRUE pour afficher les commandes exécutées
}

iris_outlier <- iris %>%
  add_row(Sepal.Length = 10, Sepal.Width = 3, Petal.Length = 5, Petal.Width = 1.5, Species = "setosa")


source("R/ggbetweenstats.R")

iris_outlier$Species <- as.factor(iris_outlier$Species)

ggbetweenstats(
  data = iris_outlier,
  x = Species,
  y = Sepal.Length,
  point.args = list(alpha = 0)
)

ggbetweenstats(
  data = iris_outlier,
  x = Species,
  y = Sepal.Length,
  boxplot.outliers = TRUE
)
