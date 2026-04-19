# load needed dependencies
library(statsExpressions, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

# load suggested dependencies that are non-optional
library(vdiffr, warn.conflicts = FALSE)

options(lifecycle_verbosity = "warning")

ggstatsplot_vdiffr_variant <- function() {
  # Only run visual snapshots on the stable macOS/Linux latest-release CI lanes.
  # Newer graphics stacks now differ across OS/R combinations, so skipping
  # elsewhere avoids noisy snapshot churn unrelated to ggstatsplot itself.
  supported_os <- Sys.info()[["sysname"]] %in% c("Darwin", "Linux")
  supported_r <- getRversion() >= "4.5.3"

  if (supported_os && supported_r) "r-4-5-3-plus" else NULL
}

expect_doppelganger <- function(title, fig, ..., variant = NULL) {
  if (is.null(variant)) {
    variant <- ggstatsplot_vdiffr_variant()
  }

  testthat::skip_if_not(
    !is.null(variant),
    paste(
      "vdiffr snapshots run only on macOS/Linux with R 4.5.3+",
      "to avoid renderer-specific churn on other OS and R combinations."
    )
  )

  vdiffr::expect_doppelganger(
    title = title,
    fig = fig,
    ...,
    variant = variant
  )
}
