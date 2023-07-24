# graphics engine changed in this version, and so snapshots generated on
# previous R version won't work
if (getRversion() > "4.1.0" && getRversion() < "4.4.0") {
  library(testthat)
  suppressPackageStartupMessages(library(ggstatsplot))

  test_check("ggstatsplot")
}
