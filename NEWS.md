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
  large), `decision_curve()` (per-threshold net benefit with bootstrap
  CIs), `discrimination_metrics()` (tie-aware Mann–Whitney AUROC,
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
- GitHub Actions CI matrix runs `R CMD check` on ubuntu-latest,
  macos-latest, and windows-latest across R release / devel /
  oldrel-1; a separate workflow publishes coverage via `covr` to
  Codecov.
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

- `cran-comments.md` records the surviving environment-local NOTEs.
- 9 of 23 testthat tests still skip gracefully without a PIC v1.1.0
  data symlink. The synthetic toy cohort unlocks the pure-math units
  but cannot replace the real-cohort coverage paths.
- The paper-vignette stubs for P2 and P3 will be fleshed out as the
  underlying analyses land.
