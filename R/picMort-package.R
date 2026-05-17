#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom data.table data.table
## usethis namespace: end
NULL

.datatable.aware <- TRUE

# Quiet `R CMD check` "no visible binding" notes for column names
# referenced inside `data.table[i, j, by]` expressions. These are
# resolved against the data.table at evaluation time, not in the
# package namespace.
utils::globalVariables(c(
  ".SD", ".N",
  "subject_id", "hadm_id", "icustay_id",
  "intime", "outtime", "los_hours",
  "admittime", "dischtime", "deathtime",
  "hospital_expire_flag", "icd10_code_cn", "admission_department",
  "gender", "dob", "sex", "sex_male",
  "age_years", "age_months", "admit_year",
  "is_surgical", "primary_icd_chapter",
  # G2 feature-extraction columns
  "itemid", "charttime", "valuenum", "t_hours", "var", "value", "t",
  "v_min", "v_max", "v_mean", "v_last",
  "stat", "col", "clinical_group", "variable",
  "icustay_id_first", "intime_first",
  ".outcome",
  # G3 PIM3 columns
  "sbp_first_hour_min", "sbp_used", "fio2_pao2", "base_excess",
  "mech_vent", "elective", "pupils_fixed",
  "recovery_no_bypass", "recovery_bypass",
  "risk_group", "pim3_logit", "pim3", "proxy_flags",
  "prio",
  # G4 prediction columns
  "prob_raw", "prob_calibrated", "prob_lower", "prob_upper",
  # G5/G6 evaluation columns
  "model", "threshold", "net_benefit", "type",
  "metric", "estimate", "lower", "upper",
  "subgroup", "level", "n_events", "auroc", "ici", "cal_slope",
  "observed", "logit_p", "prob"
))
