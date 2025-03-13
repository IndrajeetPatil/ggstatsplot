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
source("R/ggbetweenstats.R")

fichiers_R <- list.files(path = "R", pattern = "\\.R$", full.names = TRUE)
for (fichier in fichiers_R) {
  message("Exécution de : ", fichier)
  source(fichier, echo = TRUE)  # echo = TRUE pour afficher les commandes exécutées
}

ggbetweenstats(iris, Species, Sepal.Length)
