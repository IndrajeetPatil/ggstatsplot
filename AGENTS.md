# AGENTS.md

Project-level instructions for AI coding agents working on this repository.
This file records only what the code, `DESCRIPTION`, `Makefile`, and the
function documentation do not already tell you.

## Package overview

`ggstatsplot` creates `ggplot2` plots with statistical details in the plot. It
is a visualization frontend for `statsExpressions`: statistical computation
belongs there, and plotting functions here should delegate to it rather than
duplicate statistical logic.

`grouped_*` variants map the corresponding plotting function across a grouping
variable and combine the plots with `patchwork`; follow the existing `purrr` and
`patchwork::wrap_plots()` pattern. `ggcoefstats()` has no grouped variant.

## Validation gate

The repository prompts and skills refer to this as the full validation gate:

```bash
air format . --check
make lint
make hooks
make check
git diff --check
```

If Air reports drift, run `air format .` and inspect the result. Run the
narrowest relevant test file first, for example
`Rscript -e 'testthat::test_local(filter = "ggbetweenstats")'`. `make document`
only renders `README.Rmd`; it does not regenerate roxygen output.

## Versioning and changelog

- Development versions use a fourth-component `.9000` suffix.
- Keep the version in `DESCRIPTION`, `codemeta.json`, and the first `NEWS.md`
  heading synchronized.
- Record user-facing compatibility or behavior changes in `NEWS.md`; omit
  routine dependency, formatting, lint, test, CI, and generated-file
  maintenance.

## Testing

- `tests/testthat.R` runs tests only on Linux and macOS, so there are no
  Windows snapshot variants.
- Codecov requires 100% project and patch coverage.
- Set seeds before tests that use random or Bayesian computations. Suppress
  warnings only when a test intentionally exercises a warning-producing path.
- Plot output is covered by `vdiffr` snapshots (`expect_doppelganger()`) in
  `tests/testthat/_snaps/`. A few use `variant =` for platform (`darwin/`,
  `linux/`) or R-version (`r-4.7/`) differences; add a variant only when the
  difference is confirmed to be renderer-specific.
- Treat broad snapshot diffs after dependency updates as likely renderer churn.
  Compare the old and new dependency builds under the same R and graphics
  stack; if both reproduce the same SVG changes, restore the repository
  snapshots rather than committing them. Confirm legitimate baseline updates
  against the snapshot artifact the R-CMD-check workflow uploads on failure.

## Code conventions

- Use the base R pipe (`|>`), not `%>%`, and snake_case names.
- Use `@autoglobal` from `roxyglobals` where appropriate.

## Roxygen documentation

- After changing roxygen comments, run `Rscript -e 'roxygen2::roxygenise()'`
  and commit the generated `NAMESPACE`, `man/*.Rd`, `API` (from `pkgapi`), and
  `R/globals.R` (from `roxyglobals`). Do not edit generated files by hand.
- `pkgapi` is not on CRAN; install it with `pak::pak("r-lib/pkgapi")`.
- Regenerate with the roxygen2 release in `Config/roxygen2/version`. A newer
  release rewrites that field and may reformat `NAMESPACE`; bump it only
  deliberately, together with the regenerated output.
- Several parameters are inherited from `statsExpressions` via
  `@inheritParams`, so generated `.Rd` text depends on the installed version.
  Regenerate against the version `DESCRIPTION` requires (its GitHub development
  version while listed under `Remotes`, otherwise its CRAN release).
- Shared documentation lives in fragments; edit the fragment, not the generated
  output. `man/rmd-fragments/*.Rmd` are pulled into function docs with roxygen
  `child=` chunks and into `vignettes/web_only/` articles with `asis` file
  chunks. `man/md-fragments/*.md` are included by vignettes with `asis` file
  chunks. In articles, use `asis` file chunks rather than `child=`: pkgdown
  renders them from a temporary file, so relative child paths do not resolve.
  The closing sections shared by the articles live in
  `vignettes/web_only/_footer.Rmd`, which contains R code and is therefore
  knitted from a chunk with `knitr::knit_child(text = readLines(...))`.

## Repository skills and prompts

Task-specific procedures live in `.agents/skills/<name>/SKILL.md`. Read one
only when the task matches it:

- `create-release`: prepare, submit, resume, or publish a CRAN release.
- `update-dependencies`: update dependencies, change the minimum R version, or
  add, remove, or move a dependency.
- `maintain-ci`: change or debug workflows under `.github/workflows/`.

User-invoked prompts (`address-review.md` and `simplify-codebase.md`) live in
`.github/prompts/`. Keep each topic in exactly one place: this file for
every-session rules, a skill or prompt for task-specific procedures.

## Continuous integration

Workflows under `.github/workflows/` are thin callers of shared reusable
workflows; the `maintain-ci` skill covers changing them.

- Reference `IndrajeetPatil/workflows` reusable workflows as `@main`, never a
  commit SHA. The owner maintains them and is the only person who can
  contribute to this repository, so there is no external-contributor risk, and
  `@main` keeps it on the latest version. Code scanners (CodeQL
  `actions/unpinned-tag`, zizmor `unpinned-uses`) flag this by design: do not
  report it or propose a pin; dismiss such alerts as "won't fix" with this
  reason.

## Benchmarks

See [benchmarks/README.md](benchmarks/README.md) for API coverage policy,
workload catalogue, local execution, and interpretation of CodSpeed results.

## Pull requests

Open pull requests as ready for review. After pushing, wait for CI and fix any
failures. Squash-merge only after all required builds are green and Codex has
approved the current head, with no unresolved findings and the reviewed commit
matching the head. Because only the repository owner can contribute, a coding
agent may use admin privileges to squash-merge when GitHub's required human
approval is the only remaining blocker. Never use admin privileges to bypass
builds or Codex approval.
