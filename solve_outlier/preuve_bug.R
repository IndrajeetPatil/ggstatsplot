getwd()
devtools::load_all()

# Preuve du bug

# Default graph (points are visible, so no duplicate outliers)
ggbetweenstats(mtcars, cyl, mpg)

# The disappearance of dots and violins is forced : this plot should show outliers for the '8' cylinder group, but it doesn't.
ggbetweenstats(
  data = mtcars,
  x = cyl,
  y = mpg,
  point.args = list(alpha = 0),
  violin.args = list(width = 0, linewidth = 0, colour = NA),
)

