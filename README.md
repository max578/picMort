# picMort

> Reproducible Paediatric ICU Mortality Benchmark on PIC v1.1.0 —
> calibration-first, decision-curve-evaluated, Bayesian
> regularised-horseshoe with patient-level uncertainty.

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE.md)

`picMort` is the methodological substrate for a three-paper paediatric
ICU mortality + LLM series on the open-access **Paediatric Intensive
Care (PIC) v1.1.0** database from Tongji Hospital, Shanghai (released
2020 via PhysioNet; Zeng et al., *Scientific Data* 2020). The package
freezes a single cohort specification, exposes a leakage-audited
T+24 h feature panel, reconstructs PIM3 with documented proxies for
inputs PIC does not expose, and ships a **calibration-first**
evaluation suite that puts integrated calibration index and decision-
curve net benefit alongside the conventional discrimination metrics.

The package is a **first public experimental release** (`0.1.0`). The
first companion paper (P1) is in near-submission state for *Pediatric
Critical Care Medicine*; the methodology is reflected here but not yet
peer-reviewed. Public APIs are marked experimental until the paper
series has cleared review.

## Why calibration-first

A clinical-prediction model with strong AUROC but poor calibration
can produce *negative* net benefit at decision thresholds — i.e., it
is actively worse than treating no patient. The recent methodological
consensus (Van Calster et al., *BMC Medicine* 2019; Vickers et al.,
*BMJ* 2016; Collins et al., TRIPOD+AI, *BMJ* 2024) is that
discrimination alone is not enough to clear a model for clinical
translation. `picMort` is built around that consensus: every model
fit returns a calibration suite and a decision-curve grid by default,
and the Bayesian regularised-horseshoe fit (Carvalho et al.,
*Biometrika* 2010; Piironen & Vehtari, *Electron. J. Stat.* 2017)
delivers per-patient 95 % credible intervals natively from the
posterior.

## Installation

The package is not on CRAN or r-universe yet. Install the development
version directly from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("max578/picMort")
```

Required dependencies are listed in `DESCRIPTION` under `Imports:`
(notably `data.table`, `glmnet`, `xgboost`, `recipes`, `rsample`,
`ggplot2`). `brms`, `rstan`, `dcurves`, `yardstick`, `probably`,
and the targets stack are in `Suggests:` and loaded only when the
relevant entry points are called. R `>= 4.2` is required.

## Data access

The PIC v1.1.0 database is **open-access** via PhysioNet under a
registered data-use agreement at
<https://physionet.org/content/picdb/1.1.0/>. After credentialing,
mount the CSVs at a stable path and point `picMort` at that directory
via the `PICMORT_DATA_DIR` environment variable. The development-tree
fallback is a `data_links/pic_v110/` symlink; see `?pic_paths`.
Patient data must not be redistributed with this package and is not
included.

## Quick start

``` r
library(picMort)

# resolve the PIC v1.1.0 CSV paths; errors clearly if any are missing
paths <- pic_paths()

# assemble the frozen cohort (first ICU stay per patient,
# age 0-18 y, ICU LOS >= 24 h)
cohort <- build_cohort(paths)

# extract T+24h feature panel + missingness indicators
feat <- build_features(cohort, paths, window_hours = 24)

# leakage audit — fatal stop if any feature crosses the window
audit_no_leakage(feat$dict, window_hours = 24)

# 70/30 stratified train/test split
split <- make_train_test_split(feat, prop = 0.7)

# Bayesian regularised-horseshoe (production: 4 chains x 2000 iter)
fit <- fit_bayes_horseshoe(feat, split$train_idx, chains = 4, iter = 2000,
                           adapt_delta = 0.99)

# headline metrics on the held-out fold
x_test <- feat$x[split$test_idx, ]
y_test <- feat$y[split$test_idx]
preds  <- predict_mortality(fit, x_test)

calibration_suite(preds$prob_raw, y_test, seed = 2026)
decision_curve(list(bayes_horseshoe = preds$prob_raw), y_test,
               thresholds = c(0.05, 0.10, 0.20))
discrimination_metrics(list(bayes_horseshoe = preds$prob_raw), y_test,
                       seed = 2026)
```

Worked examples and the full P1 analytical pipeline live in the
vignettes:

- `vignette("cohort_spec", package = "picMort")` — the cohort
  contract.
- `vignette("paper1_baseline", package = "picMort")` — P1 baseline
  (calibration-first comparison of four models).
- `vignette("paper2_inference", package = "picMort")` — P2 (stub;
  inductive-CI inference, lead-author work in progress).
- `vignette("paper3_fusion", package = "picMort")` — P3 (stub;
  structured + Chinese clinical-note fusion, gated on the
  `EMR_SYMPTOMS` audit).

## Paper-series ladder

| # | Working title | Status (2026-05-17) | Target venue |
|---|---|---|---|
| **P1** | Calibration-First Mortality Prediction in a Paediatric Intensive Care Cohort: A Bayesian Regularised-Horseshoe Approach With Patient-Level Uncertainty | Bundle v2 ready for author-side review | *Pediatric Critical Care Medicine* |
| **P2** | Globally-Optimal Confidence Intervals for Clinical Predicted Probabilities | Artefacts ready (`scripts/_outputs/{fits_g4,evaluation_g5_g6}.rds`); drafting next | *Stat Med* + clinical mirror |
| **P3** | Two-Tower Fusion of Structured Features and Chinese Clinical-Note Embeddings for Paediatric ICU Mortality Prediction | Gated on `EMR_SYMPTOMS` audit | *npj Digital Medicine* / *Lancet Digital Health* / *JAMIA* |
| **P4** *(optional)* | `picMort`: A Reproducible R Package and Benchmark for Calibration-First Paediatric ICU Outcome Prediction | Optional software paper | JOSS or *Comput Methods Programs Biomed* |

## Citation

Until the P1 paper or a JOSS software paper is accepted, please cite
the package itself:

``` r
citation("picMort")
```

The package ships a formal `inst/CITATION`; placeholders for eventual
paper DOIs will be filled as manuscripts are accepted.

## Reproducibility

- `set.seed()` is plumbed through the evaluation entry points
  (`calibration_suite()`, `discrimination_metrics()`, …); defaults
  are documented per-function.
- `_targets.R` drives the end-to-end P1 pipeline; `renv.lock` is still
  planned for the next infrastructure pass.
- Pure-math units are covered by `testthat`; data-dependent tests skip
  gracefully without registered PIC v1.1.0 source files.

## Status and gaps

This is an **experimental methods package shipped for transparency**,
not a CRAN-ready release. The package now has examples for every
exported function, a synthetic toy cohort in `inst/extdata/`, a
pkgdown configuration, and a formal citation file. Remaining release
gaps:

- No CI matrix (`.github/workflows/`) is present in this repository
  snapshot. Builds are still author-machine only until the workflow
  lands.
- No r-universe entry or JOSS submission yet.
- CRAN submission is deferred until a cohort-agnostic successor package
  extracts the calibration/DCA/PIM3 layer.

The CRAN-readiness audit verdict is **release via r-universe + JOSS
first; defer CRAN past P3 with a successor package** extracting the
cohort-agnostic calibration/DCA/PIM3 layer.

## Licence

MIT © 2026 Max Moldovan, Usman Iqbal. See [`LICENSE.md`](LICENSE.md).

## Authors and acknowledgements

- **Max Moldovan** (Adelaide; ORCID
  [0000-0001-9680-8474](https://orcid.org/0000-0001-9680-8474)) —
  author, creator, methodological lead.
- **Usman Iqbal** (Bond) — author, thesis advisor.

PIC v1.1.0 acknowledgement: Zeng, X., Yu, G., Lu, Y., et al. (2020).
PIC, a paediatric-specific intensive care database. *Scientific
Data*, 7, 14. <https://doi.org/10.1038/s41597-020-0355-4>. Data
collected at the Department of Paediatrics, Tongji Hospital, Tongji
Medical College, Huazhong University of Science and Technology,
Wuhan, China.
