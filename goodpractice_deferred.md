# goodpractice::gp() deferred findings — picMort

Tier 7 submission rehearsal, run 2026-05-17. `goodpractice::gp(path = ".")` reports two failing checks:

1. `covr` — code coverage
2. `lintr_line_length_linter` — long code lines

This file documents items deferred to a later release with rationale. All real issues (e.g. `seq_len` vs `1:length`, `T`/`F` vs `TRUE`/`FALSE`) are absent; `lintr` cross-checks confirm zero findings on `T_and_F_symbol_linter()` and `seq_linter()`.

## 1. `covr` — 38% line coverage (deferred to v0.2.0)

**Check.** "Write unit tests for all functions, and all package code in general."
**Reported coverage.** 38% (600 lines uncovered, mostly in `R/cohort.R`, `R/features.R`, `R/pim3.R`, and `R/fit.R`).
**Reason for deferral.** The bulk of uncovered code is the PIC-data-bound pipeline (`build_cohort()`, `build_features()`, `compute_pim3()`) — these require the registered PIC v1.1.0 CSVs at `data_links/pic_v110/` and cannot run on CRAN. The `testthat` suite already includes:
- gates G1 (cohort invariants), G2 (no-leakage feature audit), G3 (PIM3 face validity), G4 (fit/predict round-trip);
- a calibration-suite smoke test on synthetic data (`test-eval.R`);
- a paths test (`test-paths.R`).
Tests using PIC data are gated by `skip_if_not(dir.exists(pic_root), ...)` and execute locally but not on CRAN. Coverage as measured by `covr::package_coverage()` therefore under-counts what is actually exercised.
**Next-release plan (v0.2.0).** Add `covr::file_coverage()` runs against the toy cohort fixtures (`inst/extdata/toy_cohort.rds`) to lift covered-line accounting closer to 60-70%. Already on the v0.2 milestone list.

## 2. `lintr_line_length_linter` — 11 long lines in `R/` (deferred — readability)

After this Tier-7 sweep, 11 long-line lints remain inside `R/`. Each is alignment-driven and breaking the line would harm readability more than it helps. Test/vignette/data-raw long lines (20 additional) are not user-facing package code and inherit the same rationale.

| File:line | Length | Construct | Why kept |
|---|---|---|---|
| `R/cohort.R:153` | 83 | `sprintf("[cohort] %-32s n=%6d  (DEATHTIME present but flag=0; flag wins per spec)", ...)` | Single-line `sprintf` format string; breaking forces an ugly `paste0()` concatenation that obscures the formatted-output structure. |
| `R/features.R:22` | 86 | `list(var = "hr", label = "Heart rate / pulse", itemids = c(1003L, 1002L),` | First row of a column-aligned feature panel definition; breaking destroys the visual table that documents the 14-feature panel set. |
| `R/features.R:34` | 86 | `list(var = "glucose", label = "Glucose", itemids = c(5047L, 5223L),` | Same alignment table as above. |
| `R/features.R:36` | 86 | `list(var = "sodium", label = "Sodium", itemids = c(5230L, 5062L),` | Same. |
| `R/features.R:46` | 86 | `list(var = "creatinine", label = "Creatinine", itemids = c(5032L, 5041L),` | Same. |
| `R/features.R:253` | 83 | `add("age_months", "cohort", "static", "demographics")` | Column-aligned table-style call inside `feature_dictionary()` (positional args document the feature_dict schema). |
| `R/features.R:254` | 83 | `add("age_years", "cohort", "static", "demographics")` | Same. |
| `R/features.R:255` | 83 | `add("sex_male", "cohort", "static (1=M)", "demographics")` | Same. |
| `R/features.R:256` | 89 | `add("is_surgical", "cohort", "any SURGERY_VITAL_SIGNS row", "demographics")` | Same; transformation description is data-derived and not portable to a separate variable without obfuscation. |
| `R/features.R:257` | 82 | `add("primary_icd_chapter", "cohort", "ICD-10 chapter (admission diagnosis)",` | Same. |
| `R/features.R:274` | 83 | `add(paste0(cc, "_missing"), "derived", "1 if value missing in window, else 0", "missingness_indicator")` | Same. |

**Test/vignette long lines (20).** Mostly `skip_if_not(...)` and `expect_named(...)` calls in `tests/testthat/` (where the test description string + path makes line-length unavoidable) and two roxygen comments in `vignettes/paper{1,3}_*.Rmd` (vignette prose, not package code). These are not surfaced in `R CMD check`.

**Net effect of Tier-7 sweep.** Down from 60 lint findings (40 in `R/`, 20 elsewhere) to 31 (11 in `R/`, 20 elsewhere). The 29 fixed lines were all genuinely-improvable (could be wrapped without harming readability).

## Cross-check: real issues are absent

`lintr::lint_package('.', linters = list(T_and_F_symbol_linter(), seq_linter()))` returns **no findings**. The package already uses `TRUE`/`FALSE` and `seq_along()`/`seq_len()` throughout.

## When to revisit

At v0.2.0 release: re-run `goodpractice::gp()` and re-check whether any of the deferred items can be addressed without trading away the visual-table structure in `R/features.R`. If the feature panel grows beyond ~20 entries, switch to an external YAML / CSV-loaded panel definition and the alignment justification disappears.
