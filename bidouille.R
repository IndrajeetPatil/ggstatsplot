library(ggstatsplot)
library(ggplot2)
library(zeallot)
library(statsExpressions)
library(dplyr)
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
library(rlang)
library(statsExpressions)
library(tidyr)
library(utils)

getwd()

source("R/ggbetweenstats.R")
source("R/ggbetweenstats-helpers.R")
source("R/ggbarstats.R")

fichiers_R <- list.files(path = "R", pattern = "\\.R$", full.names = TRUE)
for (fichier in fichiers_R){
  source(fichier, echo = TRUE)
}

test <- rbind(iris, data.frame(Sepal.Length = 19, Sepal.Width = 36, Petal.Length = 23, Petal.Width = 10, Species = "setosa"))
ggstatsplot::ggbetweenstats(
  data = test,
  x = Species,
  y = Sepal.Length,
  plot.type = "boxviolin",
  point.args = list(alpha=0)
)



ggbetweenstats(
  data = test,
  x = Species,
  y = Sepal.Length,
  plot.type = "boxviolin"
)

ggstatsplot::ggbetweenstats(
  data = iris,
  x = Species,
  y = Sepal.Length,
  plot.type = "boxviolin"
)

str(iris)

summary(iris)

