getwd()
#install.packages("vdiffr")
rm(list = ls())
devtools::load_all()

# Preuve du bug

# Graphique par défaut :
# les points sont visibles, donc les outliers ne semblent pas "disparaître"
ggbetweenstats(
  data = mtcars,
  x = cyl,
  y = mpg
)

# On rend les points et les violons invisibles :
# les outliers du boxplot devraient encore apparaître pour le groupe 8,
# mais ils disparaissent.
ggbetweenstats(
  data = mtcars,
  x = cyl,
  y = mpg,
  point.args = list(alpha = 0),
  violin.args = list(width = 0, linewidth = 0, colour = NA)
)
###### avec iris

ggbetweenstats(
  data = iris,
  x = Species,
  y = Sepal.Length
)

ggbetweenstats(
  data = iris,
  x = Species,
  y = Sepal.Length,
  point.args = list(alpha = 0),
  violin.args = list(width = 0, linewidth = 0, colour = NA)
)
