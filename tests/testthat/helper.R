# load needed dependencies
library(statsExpressions, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

# load suggested dependencies that are non-optional
library(vdiffr, warn.conflicts = FALSE)

options(lifecycle_verbosity = "warning")

expect_doppelganger <- function(title, fig, ..., variant = NULL) {
  # Default snapshots are recorded on the macOS/Linux latest-release CI lanes.
  # Older R graphics engines and Windows text rendering diverge enough to create
  # noisy vdiffr churn unrelated to ggstatsplot itself, so we skip there.
  supported_os <- Sys.info()[["sysname"]] %in% c("Darwin", "Linux")
  supported_r <- getRversion() >= "4.5.3"

  testthat::skip_if_not(
    supported_os && supported_r,
    paste(
      "vdiffr snapshots run only on macOS/Linux with R 4.5.3+",
      "to avoid renderer-specific churn on other OS and R combinations."
    )
  )

  if (is.null(variant)) {
    vdiffr::expect_doppelganger(title = title, fig = fig, ...)
  } else {
    vdiffr::expect_doppelganger(title = title, fig = fig, ..., variant = variant)
  }
}
