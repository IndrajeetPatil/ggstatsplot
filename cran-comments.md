## Test environments
* local OS X install, R 3.6.0
* ubuntu 14.04 on travis-ci (devel and release)
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 notes

  - I am getting 1 `NOTE` on Windows machines about the installed size being
    bigger than 5 MB. This is because of the images included in `README`.
  - Skipping a number of tests to reduce the check time on `CRAN` machines to be
    less than 10 minutes.
  - Makes `ggstatsplot` compatible with the upcoming release of `tidyr`.
