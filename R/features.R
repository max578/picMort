## ============================================================================
## Gate G2 — feature extraction within the T+window prediction window
##
## Hard rule: no feature may be derived from any timestamp at or after
## intime + window_hours. `audit_no_leakage()` runs as a runtime invariant.
## ============================================================================

#' Canonical feature panels (vitals, labs)
#'
#' Returns the locked PIC ITEMID dictionary used by [build_features()].
#' Each entry is a list with fields:
#'   * `var`        — short variable name (used in feature column prefix)
#'   * `label`      — human-readable label (matches PIC `D_ITEMS.LABEL`)
#'   * `itemids`    — integer vector of PIC ITEMIDs that map to this variable
#'   * `unit`       — expected unit (informational; not enforced)
#'   * `clinical_group` — `"vitals"` or `"labs"`
#'
#' @keywords internal
#' @noRd
feature_panels <- function() {
  list(
    list(var = "hr",         label = "Heart rate / pulse",  itemids = c(1003L, 1002L),
         unit = "bpm",       clinical_group = "vitals"),
    list(var = "rr",         label = "Respiratory rate",    itemids = c(1004L),
         unit = "insp/min",  clinical_group = "vitals"),
    list(var = "spo2",       label = "SpO2",                itemids = c(1006L),
         unit = "%",         clinical_group = "vitals"),
    list(var = "sbp",        label = "Systolic BP",         itemids = c(1016L),
         unit = "mmHg",      clinical_group = "vitals"),
    list(var = "dbp",        label = "Diastolic BP",        itemids = c(1015L),
         unit = "mmHg",      clinical_group = "vitals"),
    list(var = "temp",       label = "Temperature",         itemids = c(1001L),
         unit = "C",         clinical_group = "vitals"),
    list(var = "glucose",    label = "Glucose",             itemids = c(5047L, 5223L),
         unit = "mmol/L",    clinical_group = "labs"),
    list(var = "sodium",     label = "Sodium",              itemids = c(5230L, 5062L),
         unit = "mmol/L",    clinical_group = "labs"),
    list(var = "potassium",  label = "Potassium",           itemids = c(5226L),
         unit = "mmol/L",    clinical_group = "labs"),
    list(var = "lactate",    label = "Lactate",             itemids = c(5227L),
         unit = "mmol/L",    clinical_group = "labs"),
    list(var = "hemoglobin", label = "Hemoglobin",          itemids = c(5099L),
         unit = "g/L",       clinical_group = "labs"),
    list(var = "platelets",  label = "Platelet count",      itemids = c(5129L),
         unit = "10^9/L",    clinical_group = "labs"),
    list(var = "creatinine", label = "Creatinine",          itemids = c(5032L, 5041L),
         unit = "umol/L",    clinical_group = "labs"),
    list(var = "urea",       label = "Urea",                itemids = c(5033L),
         unit = "mmol/L",    clinical_group = "labs")
  )
}

#' Build T+window prediction-window feature matrix
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Extracts demographics (from cohort), vitals (`CHARTEVENTS`), and
#' labs (`LABEVENTS`) within `window_hours` of ICU admission. **No
#' feature uses any timestamp at or after `intime + window_hours`.**
#' [audit_no_leakage()] runs as a runtime invariant.
#'
#' @param cohort A cohort `data.table` from [build_cohort()].
#' @param paths Output of [pic_paths()].
#' @param window_hours Prediction-window length in hours. Default 24.
#' @param feature_set One of `"simple"` (default; min/max/mean/last +
#'   missingness indicators) or `"rich"` (adds slopes / count rates;
#'   reserved for sensitivity analysis).
#'
#' @return A list with elements:
#'   * `x`      - feature `data.table` keyed on `icustay_id`
#'   * `y`      - outcome integer vector aligned to `x$icustay_id`
#'   * `dict`   - feature dictionary (`data.table`)
#'   * `audit`  - result of [audit_no_leakage()] (TRUE on pass)
#'   * `window_hours`, `feature_set`
#'
#' @examples
#' \dontrun{
#' paths    <- pic_paths()
#' cohort   <- build_cohort(paths, min_los_hours = 24L, verbose = FALSE)
#' features <- build_features(cohort, paths, window_hours = 24L)
#' dim(features$x)
#' table(features$y)
#' }
#' @export
build_features <- function(cohort, paths,
                           window_hours = 24L,
                           feature_set = c("simple", "rich")) {
  feature_set <- match.arg(feature_set)
  if (feature_set == "rich") {
    stop("`feature_set = 'rich'` is reserved for the sensitivity analysis ",
         "and not yet implemented; see vignettes/cohort_spec.Rmd.",
         call. = FALSE)
  }

  panels <- feature_panels()
  panel_lookup <- data.table::rbindlist(lapply(panels, function(p) {
    data.table::data.table(itemid = p$itemids, var = p$var,
                           clinical_group = p$clinical_group)
  }))
  data.table::setkey(panel_lookup, itemid)

  ## Lookups: keyed on icustay_id (the cohort's first-stay icustay), and
  ## a hadm_id alias to handle CHARTEVENTS rows where icustay_id is NA
  ## (1.55M of 2.28M rows in PIC v1.1.0 — recorded against hadm_id but
  ## not yet ICU-registered; treating them as cohort events when their
  ## charttime falls in the first-stay window is the standard fix).
  intime_map <- cohort[, list(icustay_id, hadm_id, intime)]
  data.table::setkey(intime_map, icustay_id)

  ## --- Vitals from CHARTEVENTS (includes orphan icustay_id rows) -------------
  ce <- data.table::fread(
    paths$chartevents,
    select = c("HADM_ID","ICUSTAY_ID","ITEMID","CHARTTIME","VALUENUM"),
    showProgress = FALSE
  )
  data.table::setnames(ce, tolower(names(ce)))
  ce <- ce[itemid %in% panel_lookup[clinical_group == "vitals", itemid]]
  ce <- ce[hadm_id %in% intime_map$hadm_id]
  ce <- merge(ce,
              intime_map[, list(hadm_id,
                                icustay_id_first = icustay_id,
                                intime_first     = intime)],
              by = "hadm_id", all.x = TRUE, allow.cartesian = TRUE)
  ## Keep direct first-stay matches and hadm-id-orphan rows; drop rows from
  ## any non-first ICU stay of the same patient.
  ce <- ce[is.na(icustay_id) | icustay_id == icustay_id_first]
  ce[, icustay_id := icustay_id_first]
  ce[, intime     := intime_first]
  ce[, c("icustay_id_first","intime_first") := NULL]
  ce[, charttime := as.POSIXct(charttime, tz = "UTC")]
  ce[, t_hours := as.numeric(difftime(charttime, intime, units = "hours"))]
  ce <- ce[t_hours >= 0 & t_hours < window_hours & !is.na(valuenum)]
  ce <- merge(ce, panel_lookup, by = "itemid", all.x = TRUE)

  vital_long <- ce[, list(icustay_id = icustay_id, var = var,
                          value = valuenum, t = charttime,
                          t_hours = t_hours)]
  vital_agg <- aggregate_panel(vital_long)

  ## --- Labs from LABEVENTS ---------------------------------------------------
  hadm_set <- unique(cohort$hadm_id)
  le <- data.table::fread(
    paths$labevents,
    select = c("HADM_ID","ITEMID","CHARTTIME","VALUENUM"),
    showProgress = FALSE
  )
  data.table::setnames(le, tolower(names(le)))
  le <- le[hadm_id %in% hadm_set]
  le <- le[itemid %in% panel_lookup[clinical_group == "labs", itemid]]
  le[, charttime := as.POSIXct(charttime, tz = "UTC")]
  hadm_to_icu <- intime_map[, list(hadm_id, icustay_id, intime)]
  le <- merge(le, hadm_to_icu, by = "hadm_id", all.x = TRUE,
              allow.cartesian = TRUE)
  le[, t_hours := as.numeric(difftime(charttime, intime, units = "hours"))]
  le <- le[t_hours >= 0 & t_hours < window_hours & !is.na(valuenum)]
  le <- merge(le, panel_lookup, by = "itemid", all.x = TRUE)

  lab_long <- le[, list(icustay_id = icustay_id, var = var,
                        value = valuenum, t = charttime,
                        t_hours = t_hours)]
  lab_agg <- aggregate_panel(lab_long)

  ## --- Wide pivot ------------------------------------------------------------
  vital_wide <- pivot_panel_wide(vital_agg,
                                 suffix_keep = c("min", "max", "mean", "last"))
  lab_wide   <- pivot_panel_wide(lab_agg,   suffix_keep = c("min","max","last"))

  ## --- Demographic features --------------------------------------------------
  demo <- cohort[, list(
    icustay_id     = icustay_id,
    age_months     = age_months,
    age_years      = age_years,
    sex_male       = as.integer(sex == "M"),
    is_surgical    = as.integer(is_surgical),
    primary_icd_chapter = primary_icd_chapter
  )]

  x <- merge(demo, vital_wide, by = "icustay_id", all.x = TRUE)
  x <- merge(x, lab_wide,    by = "icustay_id", all.x = TRUE)

  ## --- Missingness indicators (>= 5% missing) --------------------------------
  cont_cols <- setdiff(names(x),
                       c("icustay_id","age_months","age_years","sex_male",
                         "is_surgical","primary_icd_chapter"))
  miss_rates <- vapply(cont_cols, function(cc) mean(is.na(x[[cc]])), numeric(1))
  miss_cols  <- names(miss_rates)[miss_rates >= 0.05]
  for (cc in miss_cols) {
    x[, (paste0(cc, "_missing")) := as.integer(is.na(get(cc)))]
  }

  ## --- y aligned to x --------------------------------------------------------
  y_map <- cohort[, list(icustay_id, y = hospital_expire_flag)]
  data.table::setkey(y_map, icustay_id)
  y_dt  <- y_map[data.table::data.table(icustay_id = x$icustay_id),
                 on = "icustay_id"]
  y     <- y_dt$y

  ## --- Feature dictionary ----------------------------------------------------
  dict <- build_feature_dict(x, panels, miss_cols, window_hours)

  ## --- Feature-matrix / dictionary alignment invariant -----------------------
  ## `x` carries the predictor columns plus a single `icustay_id` alignment
  ## column (assigned an "id" role by [default_recipe()] and excluded by
  ## [prep_fold()] before model fitting). The dictionary must list exactly
  ## the predictor columns.
  if (!setequal(setdiff(names(x), "icustay_id"), dict$variable)) {
    stop("feature-matrix and dictionary disagree",
         call. = FALSE)
  }

  ## --- Hard runtime audit ----------------------------------------------------
  audit_pass <- audit_no_leakage(dict, raw_events = list(vital_long, lab_long),
                                 window_hours = window_hours)

  list(x = x, y = y, dict = dict, audit = audit_pass,
       window_hours = window_hours, feature_set = feature_set)
}

#' @keywords internal
#' @noRd
aggregate_panel <- function(long) {
  if (nrow(long) == 0L) {
    return(data.table::data.table(icustay_id = integer(),
                                  var = character(),
                                  v_min = numeric(),
                                  v_max = numeric(),
                                  v_mean = numeric(),
                                  v_last = numeric()))
  }
  data.table::setorder(long, icustay_id, var, t)
  long[, list(
    v_min  = min(value),
    v_max  = max(value),
    v_mean = mean(value),
    v_last = value[.N]
  ), by = list(icustay_id, var)]
}

#' @keywords internal
#' @noRd
pivot_panel_wide <- function(agg, suffix_keep) {
  if (nrow(agg) == 0L) return(data.table::data.table(icustay_id = integer()))
  long <- data.table::melt(agg, id.vars = c("icustay_id","var"),
                           variable.name = "stat", value.name = "value")
  long[, stat := sub("^v_", "", stat)]
  long <- long[stat %in% suffix_keep]
  long[, col := paste(var, stat, sep = "_")]
  data.table::dcast(long, icustay_id ~ col, value.var = "value")
}

#' @keywords internal
#' @noRd
build_feature_dict <- function(x, panels, miss_cols, window_hours) {
  rows <- list()
  add  <- function(variable, source, transformation, clinical_group) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      variable = variable, source = source,
      transformation = transformation, clinical_group = clinical_group,
      window_hours = as.integer(window_hours)
    )
  }
  add("age_months",          "cohort",     "static",                "demographics")
  add("age_years",           "cohort",     "static",                "demographics")
  add("sex_male",            "cohort",     "static (1=M)",          "demographics")
  add("is_surgical",         "cohort",     "any SURGERY_VITAL_SIGNS row", "demographics")
  add("primary_icd_chapter", "cohort",     "ICD-10 chapter (admission diagnosis)",
      "demographics")
  for (p in panels) {
    suffixes <- if (p$clinical_group == "vitals") {
      c("min", "max", "mean", "last")
    } else {
      c("min", "max", "last")
    }
    for (s in suffixes) {
      add(paste(p$var, s, sep = "_"),
          if (p$clinical_group == "vitals") "CHARTEVENTS" else "LABEVENTS",
          paste0(s, " over [0, ", window_hours, ")h"),
          p$clinical_group)
    }
  }
  for (cc in miss_cols) {
    add(paste0(cc, "_missing"),
        "derived", "1 if value missing in window, else 0", "missingness_indicator")
  }
  data.table::rbindlist(rows)
}

#' Audit feature matrix for prediction-window leakage
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Checks (i) no variable name contains a forbidden temporal token
#' (LOS, discharge, deathtime, etc.); (ii) every dictionary entry
#' declares the same `window_hours`; (iii) optional provenance check
#' on raw events confirms every observation occurred in `[0, window)`.
#'
#' @param feature_dict Feature dictionary from [build_features()].
#' @param raw_events Optional list of long-format event tables. Each
#'   table must contain either numeric `t_hours` offsets from ICU
#'   admission, or POSIXct columns `t` and `intime` from which the
#'   offset can be derived. When present, the function verifies that
#'   every event is strictly within `[0, window_hours)`.
#' @param window_hours Prediction-window length used at extraction.
#'
#' @return Invisibly `TRUE` on pass; raises a structured error on fail.
#'
#' @examples
#' # A minimal in-spec dictionary
#' ok_dict <- data.table::data.table(
#'   variable       = c("age_years", "hr_min", "spo2_min"),
#'   source         = c("cohort", "CHARTEVENTS", "CHARTEVENTS"),
#'   transformation = c("static", "min over [0,24)h", "min over [0,24)h"),
#'   clinical_group = c("demographics", "vitals", "vitals"),
#'   window_hours   = 24L
#' )
#' audit_no_leakage(ok_dict, window_hours = 24L)
#'
#' # A leaky dictionary (LOS is forbidden post-window information)
#' bad_dict <- data.table::copy(ok_dict)
#' bad_dict <- rbind(bad_dict,
#'   data.table::data.table(variable = "los_hours", source = "cohort",
#'                          transformation = "static",
#'                          clinical_group = "demographics",
#'                          window_hours = 24L))
#' tryCatch(audit_no_leakage(bad_dict, window_hours = 24L),
#'          error = function(e) conditionMessage(e))
#' @export
audit_no_leakage <- function(feature_dict, raw_events = NULL,
                             window_hours = 24L) {
  forbidden <- c("\\blos\\b", "\\blos_", "_los\\b",
                 "outtime", "discharge", "dischtime",
                 "deathtime", "post_window")
  hits <- feature_dict[grepl(paste(forbidden, collapse = "|"),
                              tolower(variable))]
  if (nrow(hits) > 0L) {
    stop("Leakage audit failed: forbidden variables present:\n - ",
         paste(hits$variable, collapse = "\n - "), call. = FALSE)
  }
  if (!all(feature_dict$window_hours == window_hours)) {
    stop("Leakage audit failed: feature dictionary has mixed window_hours; ",
         "expected ", window_hours, ".", call. = FALSE)
  }
  if (!is.null(raw_events)) {
    if (inherits(raw_events, "data.frame")) raw_events <- list(raw_events)
    bad <- data.table::rbindlist(lapply(seq_along(raw_events), function(i) {
      ev <- raw_events[[i]]
      if (is.null(ev) || nrow(ev) == 0L) return(NULL)
      ev <- data.table::as.data.table(ev)
      if ("t_hours" %in% names(ev)) {
        offset <- ev$t_hours
      } else if (all(c("t", "intime") %in% names(ev))) {
        offset <- as.numeric(difftime(ev$t, ev$intime, units = "hours"))
      } else {
        stop("Leakage audit failed: raw_events[[", i, "]] must contain ",
             "`t_hours` or both `t` and `intime`.", call. = FALSE)
      }
      bad_idx <- which(is.na(offset) | offset < 0 | offset >= window_hours)
      if (length(bad_idx) == 0L) return(NULL)
      bad_offset <- offset[bad_idx]
      finite_bad <- bad_offset[is.finite(bad_offset)]
      data.table::data.table(
        raw_event = i,
        n_bad = length(bad_idx),
        min_t_hours = if (length(finite_bad)) min(finite_bad) else NA_real_,
        max_t_hours = if (length(finite_bad)) max(finite_bad) else NA_real_
      )
    }), fill = TRUE)
    if (nrow(bad) > 0L) {
      detail <- apply(as.data.frame(bad), 1L, function(row) {
        sprintf("raw_events[[%s]]: %s outside-window rows (range %s to %s h)",
                row[["raw_event"]], row[["n_bad"]],
                row[["min_t_hours"]], row[["max_t_hours"]])
      })
      stop("Leakage audit failed: raw event timestamps outside [0, ",
           window_hours, ")h:\n - ", paste(detail, collapse = "\n - "),
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Recipe for preprocessing
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Returns a `recipes::recipe` encoding canonical preprocessing:
#' median imputation for continuous predictors, mode imputation for
#' categorical predictors, near-zero-variance drop, dummy-encoding,
#' centring + scaling. Fit on training fold; applied to test fold via
#' `prep()` / `bake()`.
#'
#' @param x Feature `data.table` from [build_features()].
#' @param y Outcome integer vector aligned to `x$icustay_id`.
#'
#' @return A `recipes::recipe` object.
#'
#' @examples
#' if (requireNamespace("recipes", quietly = TRUE)) {
#'   set.seed(20260517L)
#'   x <- data.table::data.table(
#'     icustay_id = 1:20,
#'     age_years  = runif(20, 0, 18),
#'     hr_mean    = rnorm(20, 100, 15),
#'     sex_male   = rbinom(20, 1L, 0.5)
#'   )
#'   y <- rbinom(20, 1L, 0.1)
#'   rec <- default_recipe(x, y)
#'   class(rec)
#' }
#' @export
default_recipe <- function(x, y) {
  if (!requireNamespace("recipes", quietly = TRUE)) {
    stop("`recipes` is required; install via install.packages('recipes').",
         call. = FALSE)
  }
  df <- as.data.frame(x)
  df$.outcome <- factor(y, levels = c(0L, 1L), labels = c("alive","died"))
  rec <- recipes::recipe(.outcome ~ ., data = df) |>
    recipes::update_role(icustay_id, new_role = "id") |>
    recipes::step_impute_median(recipes::all_numeric_predictors()) |>
    recipes::step_impute_mode(recipes::all_nominal_predictors()) |>
    recipes::step_nzv(recipes::all_predictors()) |>
    recipes::step_dummy(recipes::all_nominal_predictors()) |>
    recipes::step_normalize(recipes::all_numeric_predictors())
  rec
}
