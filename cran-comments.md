# cran-comments — picMort 0.1.0

> This file is written to CRAN-submission discipline standards even
> though `picMort` is **not** currently targeted for CRAN. Per the
> `/rpkg` audit at `../picMort_cran_audit/audit_report.md` (verdict:
> `NOT-CRAN-WORTHY-DOMAIN-SPECIFIC`), the release path is
> **r-universe + JOSS**; CRAN consideration is deferred to a
> hypothetical post-P3 successor package that extracts the
> cohort-agnostic calibration / DCA / PIM3 layer. The file is kept
> at the package root so that (a) future CRAN consideration starts
> with a complete paper trail, and (b) any user inspecting the
> repository sees what would be communicated to CRAN reviewers if
> the package were to be submitted today.

## Submission

- **Version:** 0.1.0 (first release).
- **Type:** New submission (hypothetical).
- **Release target:** r-universe at
  `https://max578.r-universe.dev/picMort` once the upstream
  registry repo `max578/max578.r-universe.dev` is created. The
  hypothetical CRAN target is `https://cran.r-project.org/package=picMort`.

## Test environments

| Environment | R version | OS | Status |
|---|---|---|---|
| Local | 4.5.2 (2025-10-31, "[Not] Part in a Rumble") | macOS 26.4.1, aarch64-apple-darwin20 | 0 ERROR, 0 WARNING, 2 NOTE (environment-local, documented below) |
| GitHub Actions | release / devel / oldrel-1 | ubuntu-latest, macos-latest, windows-latest | Run on every push to `main` via `.github/workflows/R-CMD-check.yaml`; results in the repository's Actions tab. |

## R CMD check results

The local `R CMD check --as-cran` run produces **0 errors, 0 warnings,
2 notes**. Both notes are environment-local artefacts that do not
appear on the CRAN winbuilder / r-universe CI farms.

### Surviving NOTEs

1. `checking for future file timestamps ... unable to verify current
   time`. The package timestamp endpoint at `worldclockapi.com` is
   unreachable from the local build host. The NOTE clears on any
   environment with that endpoint reachable (CRAN winbuilder,
   r-universe CI, Ubuntu/macOS GitHub Actions runners).

2. `checking HTML version of manual ... 'tidy' doesn't look like
   recent enough HTML Tidy`. The local macOS host ships the
   Apple-built 2006 HTML Tidy binary as `/usr/bin/tidy`. The CRAN
   and r-universe build farms use a modern (post-2015) Tidy build
   and do not surface this NOTE.

### NOTEs resolved relative to the pre-release state (audited 2026-05-17)

For the record — these were present at the audit and are now
resolved:

- `checking CRAN incoming feasibility ... URLs in DESCRIPTION ...
  404`: resolved by creating `github.com/max578/picMort`
  (commit `c027344`, 2026-05-17).
- `Title contains lowercase v1.1.0`: resolved by capitalising to
  `V1.1.0` (commit `0dc9ff1`, 2026-05-17).
- `Namespaces in Imports field not imported from: 'rlang' 'utils'`:
  resolved by dropping unused `rlang` from `Imports` and adding the
  `@importFrom utils globalVariables` directive in
  `R/picMort-package.R`.

## Downstream dependencies

There are currently no reverse dependencies on CRAN, Bioconductor,
or r-universe. As a first-release package, the revdep matrix is
empty by construction.

## Examples

All 22 exported functions carry runnable `@examples` blocks.
Functions that require the registered-DUA PIC v1.1.0 source data
wrap the data-loading step in `\dontrun{}` and additionally
exercise their pure-math behaviour against a synthetic toy cohort
shipped via `inst/extdata/toy_cohort.rds`. The Bayesian fit example
(`fit_bayes_horseshoe`) uses `\donttest{}` rather than `\dontrun{}`
because it is runnable but slower than the 5-second example budget
when run under `R CMD check --run-donttest`.

## Tests

The `testthat` suite has 23 tests across 7 files; 14 run
unconditionally, and 9 skip gracefully on environments without a
PIC v1.1.0 data symlink. Coverage is reported on every push via
the `test-coverage` workflow.

## Vignettes

Four `.Rmd` vignettes, all using `\VignetteEngine{knitr::rmarkdown}`
(per the charter mistakes-log 2026-05-08 — `.qmd` is not first-class
under `R CMD check --as-cran`):

- `cohort_spec` — frozen contract for P1, P2, P3 (v1.1.0).
- `paper1_baseline` — runnable end-to-end illustration on the
  synthetic toy cohort.
- `paper2_inference` — pre-publication scaffold (P2 analysis in
  progress).
- `paper3_fusion` — pre-publication scaffold (gated on the
  `EMR_SYMPTOMS` audit).

Each vignette renders successfully under
`rmarkdown::render(<path>, output_format = "html_document")`.

## License

MIT, with the standard `LICENSE` file naming the year and the
copyright-holder list. See `LICENSE.md` for the full text.

## Maintainer

Max Moldovan <max.moldovan@adelaide.edu.au>; ORCID
[0000-0001-9680-8474](https://orcid.org/0000-0001-9680-8474). I
agree to the CRAN maintenance commitment: I will respond to
notifications from the CRAN team within 14 days, and I will not
abandon the package once admitted to the archive (the second
commitment is precisely why the audit's verdict is "not
CRAN-worthy yet" — see `../picMort_cran_audit/audit_report.md §4`
for the reasoning).
