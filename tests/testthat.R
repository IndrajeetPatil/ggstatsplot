# graphics engine changed in R 4.4, and text rendering changed again in R 4.5,
# so snapshots generated on previous R versions won't work
if (getRversion() >= "4.5.0") {
  library(testthat)
  suppressPackageStartupMessages(library(ggstatsplot))
  test_check("ggstatsplot")
}
