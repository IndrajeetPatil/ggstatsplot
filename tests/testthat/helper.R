# load needed dependencies
library(statsExpressions, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

# load suggested dependencies that are non-optional
library(vdiffr, warn.conflicts = FALSE)

options(lifecycle_verbosity = "warning")

ggstatsplot_run_vdiffr <- function() {
  # Only run visual snapshots on the stable macOS/Linux latest-release CI lanes.
  # Newer graphics stacks now differ across OS/R combinations, so skipping
  # elsewhere avoids noisy snapshot churn unrelated to ggstatsplot itself.
  supported_os <- Sys.info()[["sysname"]] %in% c("Darwin", "Linux")
  supported_r <- getRversion() >= "4.5.3"

  supported_os && supported_r
}

expect_doppelganger <- function(title, fig, ..., variant = NULL) {
  testthat::skip_if_not(
    ggstatsplot_run_vdiffr(),
    paste(
      "vdiffr snapshots run only on macOS/Linux with R 4.5.3+",
      "to avoid renderer-specific churn on other OS and R combinations."
    )
  )

  if (is.null(variant)) {
    vdiffr::expect_doppelganger(
      title = title,
      fig = fig,
      ...
    )
  } else {
    vdiffr::expect_doppelganger(
      title = title,
      fig = fig,
      ...,
      variant = variant
    )
  }
}
