# graphics engine changed in this version, and so snapshots generated on
# previous R version won't work
if (getRversion() > "4.1.0") {
  library(testthat)
  library(ggstatsplot)

  test_check("ggstatsplot")
}
