# _targets.R -- picMort development pipeline DAG
#
# DEVELOPMENT-ONLY SCAFFOLD. This file is a developer convenience for
# iterating on the P1 / P2 / P3 series. **It is NOT the production
# reporting pipeline.** The production reporting pipeline that produces
# every number in the PICU P1 manuscript is the standalone script at
# `deliverables/pccm_submission_pkg_v3/pipeline/full_pipeline.R`, which
# replicates the gate structure below but is self-contained (no `targets`
# dependency, no `renv` initialisation required).
#
# This file is **not** part of the P1 submission package.
#
# End-to-end build (development use):
#   targets::tar_make()
#
# Gates (mirroring vignettes/paper1_baseline.Rmd):
#   G1 cohort      -> tar_target(cohort, ...)
#   G2 features    -> tar_target(features, ...)
#   G3 pim3        -> tar_target(pim3_tbl, ...)
#   G4 fits        -> tar_target(fit_enet, ...), tar_target(fit_xgb, ...)
#   G5 calibration -> tar_target(calib_*, ...)
#   G6 dca         -> tar_target(dca, ...)
#   G7 manuscript  -> rendered vignette

if (!requireNamespace("targets", quietly = TRUE)) {
  stop("Install `targets` (and optionally `tarchetypes`) before running this pipeline.")
}

library(targets)

devtools::load_all(quiet = TRUE)

tar_option_set(
  packages = c("data.table", "glmnet", "xgboost",
               "recipes", "rsample", "yardstick",
               "probably", "dcurves", "ggplot2"),
  format   = "qs2",
  seed     = 20260508L
)

list(
  # ---------------------------------------------------------------- G1
  tar_target(paths,    pic_paths(check = TRUE)),
  tar_target(cohort,   build_cohort(paths, min_los_hours = 24L)),
  tar_target(attrition, cohort_attrition(paths, min_los_hours = 24L)),

  # ---------------------------------------------------------------- G2
  tar_target(features, build_features(cohort, paths,
                                      window_hours = 24L,
                                      feature_set  = "simple")),
  tar_target(leakage_audit, audit_no_leakage(features$dict,
                                             window_hours = 24L)),

  # ---------------------------------------------------------------- G3
  tar_target(pim3_tbl, compute_pim3(cohort, paths)),
  tar_target(pim3_fv,  pim3_face_validity(pim3_tbl, cohort)),

  # ---------------------------------------------------------------- G4
  tar_target(split,    rsample::initial_split(features$x,
                                              prop = 0.7,
                                              strata = NULL)),
  tar_target(rec,      default_recipe(rsample::training(split),
                                      features$y[split$in_id])),
  tar_target(fit_enet, fit_elastic_net(rec, features$y[split$in_id])),
  tar_target(fit_xgb,  fit_xgboost(rec, features$y[split$in_id])),

  # ---------------------------------------------------------------- G5/G6
  tar_target(probs_test, list(
    pim3 = predict_mortality(pim3_tbl, rsample::testing(split)),
    enet = predict_mortality(fit_enet, rsample::testing(split)),
    xgb  = predict_mortality(fit_xgb,  rsample::testing(split))
  )),
  tar_target(calib,        calibration_suite(probs_test$enet$prob_calibrated,
                                             features$y[-split$in_id])),
  tar_target(dca,          decision_curve(probs_test,
                                          features$y[-split$in_id])),
  tar_target(discrim,      discrimination_metrics(probs_test,
                                                  features$y[-split$in_id])),
  tar_target(subgroups,    subgroup_performance(probs_test,
                                                cohort[-split$in_id]))
)
