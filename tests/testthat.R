# Tests run only on Linux and macOS to avoid maintaining Windows snapshot
# variants.
if (Sys.info()[["sysname"]] %in% c("Linux", "Darwin")) {
  library(testthat)
  suppressPackageStartupMessages(library(ggstatsplot))
  test_check("ggstatsplot")
}
