# Benchmarking

The [CodSpeed CLI](https://github.com/CodSpeedHQ/codspeed) measures the nine main
plot constructors through its executable harness. Each target constructs and
builds five plots using `ggplot2::ggplot_build()`, checking the plot class and
built layout. It includes R startup, dependency loading, fixture preparation,
and plot building; coefficient plots also include fitting a small linear model.
It does not measure graphics-device rendering or file export. Random jitter is
seeded before every call, including default statistical computations.

## Coverage policy

Benchmark the main plotting entry points rather than every public export.
Exclude meta-analysis workloads, helpers, extractors, theme functions, plot
combiners, and grouped wrappers. `ggcoefstats()` benchmarks ordinary regression
with the default `meta.analytic.effect = FALSE`; it never pools model coefficients.
Keep datasets, target names, and repetition counts stable, and update the
catalogue when the main API changes.

| Target/function | Workload |
| --- | --- |
| `ggbetweenstats()` | Between-subjects comparison of tooth growth |
| `ggwithinstats()` | Paired sleep measurements in subject order |
| `gghistostats()` | Distribution of car fuel efficiency |
| `ggdotplotstats()` | Car weights with model labels |
| `ggpiestats()` | Cylinder counts |
| `ggbarstats()` | Hair and eye counts |
| `ggscatterstats()` | Car weight versus fuel efficiency |
| `ggcorrmat()` | Three-variable car correlation matrix |
| `ggcoefstats()` | Linear-model coefficients without meta-analysis |

Use positional API calls and omit default-valued arguments to keep workloads
readable as single-line calls. Expand counted fixtures before the calls rather
than specifying a counts argument.

## Run locally

Install the checkout, then run targets from the repository root:

```sh
R CMD INSTALL .
Rscript --vanilla benchmarks/run.R ggbetweenstats
Rscript --vanilla benchmarks/run.R ggwithinstats
Rscript --vanilla benchmarks/run.R gghistostats
Rscript --vanilla benchmarks/run.R ggdotplotstats
Rscript --vanilla benchmarks/run.R ggpiestats
Rscript --vanilla benchmarks/run.R ggbarstats
Rscript --vanilla benchmarks/run.R ggscatterstats
Rscript --vanilla benchmarks/run.R ggcorrmat
Rscript --vanilla benchmarks/run.R ggcoefstats
```

These commands validate execution without collecting measurements. On supported
Linux systems with the CodSpeed CLI installed, `codspeed run --mode simulation`
measures all targets in `codspeed.yml`.

## CI and interpretation

- The `Benchmarks` caller uses `r-benchmarks.yaml` from
  `IndrajeetPatil/workflows` at `@main`.
- The shared job uses Ubuntu 24.04, R 4.6.1, and one BLAS/OpenMP thread. It
  installs the checkout and hard dependencies, and records loaded package
  versions. Dependencies are resolved from the package repository, not locked.
- Import this repository into CodSpeed through its GitHub App. Uploads use
  short-lived GitHub OIDC tokens via `id-token: write`.
- PRs record candidates; pushes to `main` establish comparison baselines.
  New or renamed targets need matching default-branch measurements.
- CPU simulation measures user-space CPU work and excludes system calls.
  These complete R commands can be flagged as dominated by system calls, so
  simulated values understate total elapsed cost. Native profiles may show
  interpreter frames rather than R function names.
- Repeat the same source and environment to investigate stability. Matching
  rounded report values alone does not establish sub-percent variance.
- Configure regression thresholds and merge requirements in CodSpeed and GitHub
  settings; the workflow requires successful execution and upload.
