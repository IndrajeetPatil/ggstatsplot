## R CMD check results

0 errors | 0 warnings | 0 notes

- This is a minor release (1.1.0).
- This is a resubmission. It fixes the URL diagnostics reported by CRAN by
  using the canonical CRAN package URL in `README.md` and the package website
  for the historical `{statsExpressions}` NEWS link.
- This release fixes the CRAN daily-check NOTE about deprecated `.Label` use
  in a test fixture by replacing it with the supported `levels` argument.
  See `NEWS.md` for a detailed changelog.

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and
dev versions of this package.

 * We saw 0 new problems

 * We failed to check 0 packages
