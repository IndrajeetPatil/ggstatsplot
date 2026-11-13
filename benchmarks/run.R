# Each command includes R startup, dependency loading, and one plot build.
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
hair_eye <- as.data.frame(datasets::HairEyeColor)
hair_eye <- hair_eye[rep(seq_len(nrow(hair_eye)), hair_eye$Freq), ]
cars <- datasets::mtcars
cars$model <- rownames(cars)

workload <- switch(
  args[[1L]],
  ggbetweenstats = function() {
    ggstatsplot::ggbetweenstats(datasets::ToothGrowth, supp, len)
  },
  ggwithinstats = function() {
    ggstatsplot::ggwithinstats(datasets::sleep, group, extra)
  },
  gghistostats = function() {
    ggstatsplot::gghistostats(datasets::mtcars, mpg)
  },
  ggdotplotstats = function() {
    ggstatsplot::ggdotplotstats(cars, wt, model)
  },
  ggpiestats = function() {
    ggstatsplot::ggpiestats(datasets::mtcars, cyl)
  },
  ggbarstats = function() {
    ggstatsplot::ggbarstats(hair_eye, Hair, Eye)
  },
  ggscatterstats = function() {
    ggstatsplot::ggscatterstats(datasets::mtcars, wt, mpg)
  },
  ggcorrmat = function() {
    ggstatsplot::ggcorrmat(datasets::mtcars, c(mpg, wt, disp))
  },
  ggcoefstats = function() {
    ggstatsplot::ggcoefstats(stats::lm(mpg ~ wt + am, datasets::mtcars))
  },
  stop("Unknown benchmark: ", args[[1L]], call. = FALSE)
)

# Plot layers and default statistical messages can use random computations.
set.seed(123L)
plot <- workload()
stopifnot(inherits(plot, "ggplot"))
built <- ggplot2::ggplot_build(plot)
stopifnot(length(built$data) > 0L, nrow(built$layout$layout) > 0L)
