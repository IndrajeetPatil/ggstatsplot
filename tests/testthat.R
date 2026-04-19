# graphics engine changed in R 4.4, and text rendering changed again in R 4.5,
# so snapshots generated on previous R versions won't work.
# Tests run only on Linux to avoid maintaining platform-specific snapshot variants.
if (getRversion() >= "4.5.0" && Sys.info()[["sysname"]] == "Linux") {
  library(testthat)
  suppressPackageStartupMessages(library(ggstatsplot))
  test_check("ggstatsplot")
}
