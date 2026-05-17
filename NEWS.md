# picMort (development version)

## Initial public commit to GitHub (2026-05-17)

First public push to `https://github.com/max578/picMort`. The package
remains pre-release; the version stays at `0.0.0.9000` until a tagged
`v0.1.0` release.

### Contents at first commit

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

### Release-readiness state

- `R CMD check --as-cran` returns 0 ERROR, 0 WARNING, 4 NOTE. Two notes
  are environment-local; two are real and queued for the first release
  (unused `rlang` import; `@importFrom utils globalVariables` directive
  to add).
- No tagged release. No `inst/extdata` toy cohort yet. No `@examples`
  on any exported function — the largest qualitative gap from the
  `/rpkg` canon.
- No CI workflow under `.github/workflows/` yet. r-universe enrolment
  and JOSS submission are explicitly deferred pending author-side
  go-ahead per `picMort_cran_audit/audit_report.md`.
