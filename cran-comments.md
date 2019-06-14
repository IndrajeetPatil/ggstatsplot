## Test environments
* local OS X install, R 3.6.0
* ubuntu 14.04 on travis-ci (devel and release)
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

  - On both LINUX and Windows architectures, the CHECK returns one `NOTE`
    because the `help` sub-directory is above the `1MB` threshold. This is
    because graphics images in the `README` file are being saved in this
    directory. I can reduce the `dpi` to get rid of this `NOTE`, but this would
    lead to loss of important details in the plots that I would want the
    user/reader to appreciate and I would therefore like to retain the current
    high-res (`dpi = 200`) images.
  - Replaced `\dontrun{}` with ` \donttest{}` in all `.Rd` files.
  - All `CRAN`-releated URL linkes in the recommended format now.
