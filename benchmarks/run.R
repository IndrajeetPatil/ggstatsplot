# Each command includes R startup, dependency loading, and five plot builds.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
cars <- datasets::mtcars
cars$model <- rownames(cars)

workload <- switch(
  args[[1L]],
  ggbetweenstats = function() {
    ggstatsplot::ggbetweenstats(
      data = datasets::ToothGrowth,
      x = supp,
      y = len,
      bf.message = FALSE
    )
  },
  ggwithinstats = function() {
    ggstatsplot::ggwithinstats(
      data = datasets::sleep,
      x = group,
      y = extra,
      subject.id = ID,
      bf.message = FALSE
    )
  },
  gghistostats = function() {
    ggstatsplot::gghistostats(
      data = datasets::mtcars,
      x = mpg,
      bf.message = FALSE
    )
  },
  ggdotplotstats = function() {
    ggstatsplot::ggdotplotstats(
      data = cars,
      x = wt,
      y = model,
      bf.message = FALSE
    )
  },
  ggpiestats = function() {
    ggstatsplot::ggpiestats(
      data = datasets::mtcars,
      x = cyl,
      bf.message = FALSE
    )
  },
  ggbarstats = function() {
    ggstatsplot::ggbarstats(
      data = as.data.frame(datasets::HairEyeColor),
      x = Hair,
      y = Eye,
      counts = Freq,
      bf.message = FALSE
    )
  },
  ggscatterstats = function() {
    ggstatsplot::ggscatterstats(
      data = datasets::mtcars,
      x = wt,
      y = mpg,
      bf.message = FALSE
    )
  },
  ggcorrmat = function() {
    ggstatsplot::ggcorrmat(data = datasets::mtcars, cor.vars = c(mpg, wt, disp))
  },
  ggcoefstats = function() {
    ggstatsplot::ggcoefstats(
      x = stats::lm(mpg ~ wt + am, data = datasets::mtcars),
      meta.analytic.effect = FALSE,
      bf.message = FALSE
    )
  },
  stop("Unknown benchmark: ", args[[1L]], call. = FALSE)
)

for (iteration in seq_len(5L)) {
  # Plot layers can use random jitter; use the same realization on every call.
  set.seed(123L)
  plot <- workload()
  stopifnot(inherits(plot, "ggplot"))
  built <- ggplot2::ggplot_build(plot)
  stopifnot(length(built$data) > 0L, nrow(built$layout$layout) > 0L)
}
