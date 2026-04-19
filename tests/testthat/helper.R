# load needed dependencies
library(statsExpressions, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

# load suggested dependencies that are non-optional
library(vdiffr, warn.conflicts = FALSE)

options(lifecycle_verbosity = "warning")

ggstatsplot_vdiffr_variant <- function() {
  # Newer R graphics stacks render these snapshots differently from the
  # baseline used by the oldrel CI jobs.
  if (getRversion() < "4.5.3") {
    NULL
  } else if (.Platform$OS.type == "windows") {
    "windows-r-4-5-3-plus"
  } else {
    "r-4-5-3-plus"
  }
}

expect_doppelganger <- function(title, fig, ..., variant = NULL) {
  if (is.null(variant)) {
    variant <- ggstatsplot_vdiffr_variant()
  }

  vdiffr::expect_doppelganger(
    title = title,
    fig = fig,
    ...,
    variant = variant
  )
}
