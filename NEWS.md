# picMort 0.1.1 (2026-05-17)

## Patch — post-release audit fixes

A second independent audit performed against the v0.1.0 release surfaced
one runtime bug, two correctness mismatches, two robustness gaps, and a
small documentation drift. All are closed in this patch; the public API
surface is unchanged except for the one additive item under §New
features.

### Bug fixes

- `vignettes/paper1_baseline.Rmd` could enter a branch where
  `predict_mortality(fit_bh, ...)` ran even though `fit_bh` had not been
  created. The Bayesian fit is now gated behind the
  `PICMORT_RUN_VIGNETTE_BAYES=true` environment variable, and `fit_bh`
  is initialised to `NULL` in the setup chunk so the no-Bayes path is
  well defined.
- `audit_no_leakage()` now matches its documentation: it inspects the
  supplied raw event offsets, not just feature names and
  `window_hours`. The function accepts numeric `t_hours` or derives
  offsets from `t` and `intime`. `build_features()` was updated to pass
  raw vitals and labs with `t_hours`.
- `simple_auroc()` now uses rank-based Mann-Whitney logic with
  averaged ties. A regression test covers the tied two-row case that
  previously yielded an incorrect AUROC.

### Robustness

- Calibration components (`calibration_suite()`) return `NA_real_`
  where the estimate is not numerically defensible (very small
  subgroups, degenerate fits) rather than emitting noisy `loess()` and
  `glm()` warnings during tests.

### New features

- `PICMORT_DATA_DIR` environment variable. `pic_paths()` now resolves
  PIC source files from `PICMORT_DATA_DIR` when set, otherwise from
  the development-tree fallback `data_links/pic_v110/`. A regression
  test covers this path-resolution branch.

### Documentation and packaging

- `Suggests:` adds `BH` because the P1 vignette explicitly checks for
  it alongside `brms` and `rstan` (the C++ Boost headers used by the
  Stan toolchain).
- `.Rbuildignore` now excludes `.DS_Store`, locally generated
  tarballs, and the `picMort.Rcheck/` directory.
- `README`, `NEWS`, and `cran-comments` were synced with the actual
  repository state (no longer claim a pre-`0.1.0` state or a CI
  workflow that does not yet exist in the package directory).
- `goodpractice_deferred.md` refreshed to current coverage and
  line-length audit numbers.

### Verification at v0.1.1

- `devtools::test()` — `FAIL 0 | WARN 0 | SKIP 1 | PASS 91`. The skip
  is intentional: the Stan / brms smoke test runs only when
  `PICMORT_TEST_BAYES=true`.
- `rcmdcheck::rcmdcheck(args = c("--no-manual", "--no-build-vignettes"))`
  — `0 errors | 0 warnings | 0 notes`.

# picMort 0.1.0 (2026-05-17)

## First public release

First tagged release at `https://github.com/max578/picMort`. The
release closes Tiers 0–7 of the `/rpkg` audit recorded in
`../picMort_cran_audit/audit_report.md` (audit date 2026-05-17), with
the explicit exceptions documented in §Release-readiness state below.
The package is published for installation via r-universe and direct
GitHub install; CRAN submission is deferred per the audit verdict
until a cohort-agnostic successor package is extracted post-P3.

### Contents at v0.1.0

- **Frozen cohort specification for PIC v1.1.0** — vignette `cohort_spec`
  is the contract shared by P1, P2, and P3 in the PICU mortality + LLM
  paper series. First ICU stay per patient, age 0–18 years, ICU LOS ≥ 24
  hours.
- **Prediction-window-locked feature extraction at T+24h post-ICU admit**
  with a runtime leakage audit (`audit_no_leakage()`).
- **PIM3 reconstruction** (`compute_pim3()`, `pim3_risk_group()`,
  `pim3_face_validity()`) with documented proxies for unrecoverable
  first-hour inputs. PIM3 is positioned as a sentinel marker of where
  the field standard cannot be cleanly reconstructed from PIC v1.1.0,
  not as a competitive baseline.
- **Calibration-first evaluation suite** — `calibration_suite()`
  (slope, intercept, integrated calibration index, calibration-in-the-
  large), `decision_curve()` (per-threshold net benefit),
  `discrimination_metrics()` (tie-aware Mann–Whitney AUROC,
  AUPRC, Brier score and Brier skill score), `subgroup_performance()`.
- **Three model fits** — penalised logistic regression (`fit_elastic_net()`
  via glmnet), gradient-boosted trees (`fit_xgboost()`), and Bayesian
  regularised-horseshoe logistic regression (`fit_bayes_horseshoe()`
  via brms / Stan).
- **testthat coverage** of pure-math units (calibration, decision-curve,
  ICD-10 chapter mapping, PIM3 risk groups); 9 of 23 tests skip
  gracefully without the registered-DUA PIC v1.1.0 data symlink.

### Release-readiness state at v0.1.0

- `R CMD check --as-cran` runs clean on the build farm (0 ERROR,
  0 WARNING; surviving NOTEs are environment-local and documented in
  `cran-comments.md`).
- `@examples` blocks present on every exported function. Functions
  that require the registered-DUA PIC v1.1.0 data wrap their example
  in `\dontrun{}` and additionally exercise pure-math behaviour
  against the synthetic toy cohort at `inst/extdata/toy_cohort.rds`.
- `lifecycle::badge("experimental")` applied to every exported
  function; the lifecycle ladder is published in `API_STABILITY.md`.
- GitHub Actions CI is not yet present in this repository snapshot;
  local `R CMD check` and `testthat` are the release gates until the
  workflow is added.
- `pkgdown` site configuration ships in `_pkgdown.yml`; render is
  author-driven (no auto-deploy from CI in this release).
- `inst/CITATION` lists the package, with placeholders for the
  eventual P1 (PCCM) and JOSS DOIs.
- The three paper vignettes (`paper1_baseline`, `paper2_inference`,
  `paper3_fusion`) flesh out the substrate at v0.1.0; P1 is runnable
  end-to-end on the toy cohort, P2 and P3 are honest pre-publication
  scaffolds.

### Explicit non-goals at v0.1.0

- **No r-universe enrolment.** The package is ready for it (the
  upstream registry repo at `max578/max578.r-universe.dev` plus a
  one-line `packages.json` is the only remaining step), but
  enrolment is deferred pending author-side go-ahead per the
  `/rpkg-walkthrough` W7 / charter §1 publication discipline.
- **No JOSS submission.** `paper.md` is not drafted in this release.
- **No CRAN submission.** Per the audit verdict, CRAN consideration
  returns only after a cohort-agnostic successor package is
  extracted post-P3.

### Known gaps documented for the next minor release

- `cran-comments.md` records historically observed environment-local
  NOTEs for future CRAN-style rehearsals.
- 9 of 26 testthat tests still skip gracefully without a PIC v1.1.0
  data symlink. The synthetic toy cohort unlocks the pure-math units
  but cannot replace the real-cohort coverage paths.
- The paper-vignette stubs for P2 and P3 will be fleshed out as the
  underlying analyses land.
- Add CI (`R CMD check` matrix plus coverage) before any external
  release announcement beyond GitHub / author-side installs.
