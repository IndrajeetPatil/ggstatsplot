## Test environments
* local Windows install, R 3.6.1
* ubuntu 14.04 on travis-ci (devel and release)
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 notes

  - Makes `ggstatsplot` compatible with the new release of `skimr 2.0`.
  - Some of the examples might take more than 5 seconds to run. Haven't skipped
    them because I am skipping all tests in daily `CRAN` checks and so it's
    important that at least the examples are checked on a daily basis.
