# AGENTS.md

Project-level instructions for AI coding agents working on this
repository. This file records only what the code, `DESCRIPTION`,
`Makefile`, and the function documentation do not already tell you.

## Package overview

`ggstatsplot` creates `ggplot2` plots with statistical details in the
plot. It is a visualization frontend for `statsExpressions`: statistical
computation belongs there, and plotting functions here should delegate
to it rather than duplicate statistical logic.

`grouped_*` variants map the corresponding plotting function across a
grouping variable and combine the plots with `patchwork`; follow the
existing `purrr` and
[`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
pattern.
[`ggcoefstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggcoefstats.md)
has no grouped variant.

## Validation gate

The repository prompts and skills refer to this as the full validation
gate:

``` bash
air format . --check
make lint
make hooks
make check
git diff --check
```

If Air reports drift, run `air format .` and inspect the result. Run the
narrowest relevant test file first, for example
`Rscript -e 'testthat::test_local(filter = "ggbetweenstats")'`.
`make document` only renders `README.Rmd`; it does not regenerate
roxygen output.

## Versioning and changelog

- Development versions use a fourth-component `.9000` suffix.
- Keep the version in `DESCRIPTION`, `codemeta.json`, and the first
  `NEWS.md` heading synchronized.
- Record user-facing compatibility or behavior changes in `NEWS.md`;
  omit routine dependency, formatting, lint, test, CI, and
  generated-file maintenance.

## Testing

- `tests/testthat.R` runs tests only on Linux and macOS, so there are no
  Windows snapshot variants.

- CI requires 100% line and branch coverage (covr’s
  `covr.branch_coverage`), and Codecov requires 100% project and patch
  coverage. A [`switch()`](https://rdrr.io/r/base/switch.html) without a
  default leaves an untaken “no match” branch, so make its last case the
  default or use a lookup vector.

- A separate CI job requires 100% coverage from examples and vignettes
  alone (`covr::package_coverage(type = c("examples", "vignettes"))`).
  Most `# nocov` markers exclude code that the tests cover but the
  examples do not, so do not remove a marker because the test suite
  covers its code.

- Set seeds before tests that use random or Bayesian computations.
  Suppress warnings only when a test intentionally exercises a
  warning-producing path.

- Plot output is covered by `vdiffr` snapshots (`expect_doppelganger()`)
  in `tests/testthat/_snaps/`. A few use `variant =` for platform
  (`darwin/`, `linux/`) or R-version (`r-4.7/`) differences; add a
  variant only when the difference is confirmed to be renderer-specific.

- Treat broad snapshot diffs after dependency updates as likely renderer
  churn. Compare the old and new dependency builds under the same R and
  graphics stack; if both reproduce the same SVG changes, restore the
  repository snapshots rather than committing them. Confirm legitimate
  baseline updates against the snapshot artifact the R-CMD-check
  workflow uploads on failure.

## Code conventions

- Use the base R pipe (`|>`), not `%>%`, and snake_case names.
- Use `@autoglobal` from `roxyglobals` where appropriate.

## Roxygen documentation

- After changing roxygen comments, run
  `Rscript -e 'roxygen2::roxygenise()'` and commit the generated
  `NAMESPACE`, `man/*.Rd`, `API` (from `pkgapi`), and `R/globals.R`
  (from `roxyglobals`). Do not edit generated files by hand.
- `pkgapi` is not on CRAN; install it with `pak::pak("r-lib/pkgapi")`.
- Regenerate with the roxygen2 release in `Config/roxygen2/version`. A
  newer release rewrites that field and may reformat `NAMESPACE`; bump
  it only deliberately, together with the regenerated output.
- Several parameters are inherited from `statsExpressions` via
  `@inheritParams`, so generated `.Rd` text depends on the installed
  version. Regenerate against the version `DESCRIPTION` requires (its
  GitHub development version while listed under `Remotes`, otherwise its
  CRAN release).
- Shared documentation lives in fragments; edit the fragment, not the
  generated output. `man/rmd-fragments/*.Rmd` are pulled into function
  docs with roxygen `child=` chunks and into `vignettes/web_only/`
  articles with `asis` file chunks. `man/md-fragments/*.md` are included
  by vignettes with `asis` file chunks. In articles, use `asis` file
  chunks rather than `child=`: pkgdown renders them from a temporary
  file, so relative child paths do not resolve. The closing sections
  shared by the articles live in `vignettes/web_only/_footer.Rmd`, which
  contains R code and is therefore knitted from a chunk with
  `knitr::knit_child(text = readLines(...))`.

## Repository skills and prompts

Task-specific procedures live in `.agents/skills/<name>/SKILL.md`. Read
one only when the task matches it:

- `create-release`: prepare, submit, resume, or publish a CRAN release.
- `update-dependencies`: update dependencies, change the minimum R
  version, or add, remove, or move a dependency.
- `maintain-ci`: change or debug workflows under `.github/workflows/`.

User-invoked prompts (`address-review.md`, `simplify-codebase.md`, and
`update-deps.md`) live in `.github/prompts/`. Keep each topic in exactly
one place: this file for every-session rules, a skill or prompt for
task-specific procedures.

## Continuous integration

Workflows under `.github/workflows/` are thin callers of shared reusable
workflows; the `maintain-ci` skill covers changing them.

- Reference `IndrajeetPatil/workflows` reusable workflows as `@main`,
  never a commit SHA. The owner maintains them and is the only person
  who can contribute to this repository, so there is no
  external-contributor risk, and `@main` keeps it on the latest version.
  Code scanners (CodeQL `actions/unpinned-tag`, zizmor `unpinned-uses`)
  flag this by design: do not report it or propose a pin; dismiss such
  alerts as “won’t fix” with this reason.

## Benchmarks

See
[benchmarks/README.md](https://www.indrapatil.com/ggstatsplot/benchmarks/README.md)
for API coverage policy, workload catalogue, local execution, and
interpretation of CodSpeed results.

## Pull requests

Follow these rules without relying on any instructions outside this
repository:

- Write every commit message and PR title as a Conventional Commit,
  `type(scope): description`, lowercase and imperative, with an allowed
  type (`build`, `chore`, `ci`, `docs`, `feat`, `fix`, `perf`,
  `refactor`, `revert`, `style`, `test`). Keep the PR title and body in
  sync with the net diff.

- Open every PR ready for review with `gh pr create`, never as a draft.

- Never add agent or tool attribution to commits, PRs, or comments.

- After opening or pushing, wait for hosted CI to finish. Inspect failed
  jobs, fix the cause, run the local checks (`make check` and
  `make hooks`), push, and repeat. Skipped, queued, or failing required
  checks are not success.

- If an automatic AI review (Codex, for the owner) runs on the PR, wait
  for its result instead of requesting one. Fix merited findings, reply
  to and resolve addressed threads, and continue until the review
  approves the current head with no unresolved findings. Never write
  `@codex` anywhere in a PR, even in backticks: it cancels the automatic
  review. Say “a Codex review request” instead.

- If Codex reports exhausted usage, leave the PR open and report it; no
  other reviewer or silence counts as approval. Once usage refreshes,
  comment `@codex review` once; only this case needs the comment.

- Once CI is green and the review gate is met, squash-merge, then verify
  the merged state. Recheck the head, threads, and checks immediately
  before merging, and never bypass branch protections. Because only the
  repository owner can contribute, a coding agent may use admin
  privileges to squash-merge when GitHub’s required human approval is
  the only remaining blocker. Never use admin privileges to bypass
  builds or Codex approval.

- Update a PR branch from `main` only to resolve conflicts or pick up a
  change the PR depends on, never just to bring it up to date.

- Report local validation, hosted CI, review, and merge state
  separately. If an external blocker stops completion, leave the PR open
  and say so.
