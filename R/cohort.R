#' Build the canonical study cohort
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Constructs the canonical study cohort per `vignettes/cohort_spec.Rmd`:
#' first ICU stay per patient; age 0-18 y at admission; valid ICU
#' admit/discharge timestamps; in-hospital mortality outcome attached.
#'
#' Cohort assembly is the single source of truth for the calibration-first
#' analysis. Deviations are forbidden; sensitivity analyses operate on the
#' same base cohort with documented filters.
#'
#' @param paths Output of [pic_paths()].
#' @param min_los_hours Minimum ICU length-of-stay in hours required for
#'   the prediction-window framing. Default 24 (T+24h lock). Set to 12
#'   for the T+12h sensitivity arm.
#' @param prediction_window_hours Prediction window in hours. Used by
#'   `is_surgical` to filter `SURGERY_VITAL_SIGNS` evidence to rows that
#'   occur strictly before `T0 + prediction_window_hours`. Without this
#'   filter, surgery evidence appearing only after the prediction-window
#'   close leaks future information. Default = `min_los_hours` (T+24h
#'   main analysis); pass `12L` for the T+12h sensitivity arm.
#' @param verbose Logical. Emit cohort-attrition log lines.
#'
#' @return A `data.table` keyed by `subject_id`, `hadm_id`, `icustay_id`.
#'   The cohort table carries an `"attrition"` attribute documenting the
#'   exclusion cascade, accessed via [cohort_attrition()].
#'
#' @examples
#' # `build_cohort()` reads the registered PIC v1.1.0 CSVs. A pre-built
#' # synthetic 80-row toy cohort with the same schema ships in
#' # `inst/extdata/toy_cohort.rds` for examples and tests.
#' toy <- readRDS(system.file("extdata", "toy_cohort.rds",
#'                            package = "picMort"))
#' nrow(toy)
#' names(toy)
#'
#' \dontrun{
#' paths  <- pic_paths()
#' cohort <- build_cohort(paths, min_los_hours = 24L, verbose = FALSE)
#' assert_cohort_invariants(cohort)
#' }
#' @importFrom data.table fread setnames setorder uniqueN setattr `:=` `.SD`
#' @export
build_cohort <- function(paths, min_los_hours = 24L,
                         prediction_window_hours = min_los_hours,
                         verbose = TRUE) {
  log_step <- function(steps, step, n,
                       excluded = NA_integer_,
                       reason = NA_character_) {
    if (isTRUE(verbose)) {
      message(sprintf("[cohort] %-32s n=%6d  (%s)", step, n,
                      if (is.na(reason)) "" else reason))
    }
    c(steps, list(data.table::data.table(step = step, n = n,
                                         excluded = excluded,
                                         reason = reason)))
  }

  steps <- list()

  adm <- data.table::fread(paths$admissions, showProgress = FALSE)
  pat <- data.table::fread(paths$patients,   showProgress = FALSE)
  icu <- data.table::fread(paths$icustays,   showProgress = FALSE)
  data.table::setnames(adm, tolower(names(adm)))
  data.table::setnames(pat, tolower(names(pat)))
  data.table::setnames(icu, tolower(names(icu)))

  for (col in intersect(c("admittime","dischtime","deathtime"), names(adm))) {
    adm[[col]] <- as.POSIXct(adm[[col]], tz = "UTC")
  }
  for (col in intersect(c("dob","dod"), names(pat))) {
    pat[[col]] <- as.POSIXct(pat[[col]], tz = "UTC")
  }
  icu[, intime  := as.POSIXct(intime,  tz = "UTC")]
  icu[, outtime := as.POSIXct(outtime, tz = "UTC")]

  steps <- log_step(steps, "raw_icu_stays", nrow(icu), reason = "ICUSTAYS rows")

  before <- nrow(icu)
  icu <- icu[!is.na(intime) & !is.na(outtime)]
  steps <- log_step(steps, "valid_icu_timestamps", nrow(icu),
                    excluded = before - nrow(icu),
                    reason = "missing INTIME or OUTTIME")

  before <- nrow(icu)
  icu <- icu[outtime > intime]
  steps <- log_step(steps, "ordered_icu_timestamps", nrow(icu),
                    excluded = before - nrow(icu),
                    reason = "OUTTIME <= INTIME")

  data.table::setorder(icu, subject_id, intime)
  before <- nrow(icu)
  icu_first <- icu[, .SD[1L], by = subject_id]
  steps <- log_step(steps, "first_stay_per_patient", nrow(icu_first),
                    excluded = before - nrow(icu_first),
                    reason = "non-first ICU stays per patient")

  adm_keep <- intersect(c("subject_id", "hadm_id",
                          "admittime", "dischtime", "deathtime",
                          "hospital_expire_flag",
                          "icd10_code_cn", "admission_department"),
                        names(adm))
  cohort <- merge(icu_first,
                  adm[, .SD, .SDcols = adm_keep],
                  by = c("subject_id","hadm_id"), all.x = TRUE)

  before <- nrow(cohort)
  cohort <- cohort[!is.na(admittime) & !is.na(dischtime)]
  steps <- log_step(steps, "valid_admit_timestamps", nrow(cohort),
                    excluded = before - nrow(cohort),
                    reason = "missing ADMITTIME or DISCHTIME")

  before <- nrow(cohort)
  cohort <- cohort[dischtime >= admittime]
  steps <- log_step(steps, "ordered_admit_timestamps", nrow(cohort),
                    excluded = before - nrow(cohort),
                    reason = "DISCHTIME < ADMITTIME")

  pat_keep <- intersect(c("subject_id","gender","dob"), names(pat))
  cohort <- merge(cohort,
                  pat[, .SD, .SDcols = pat_keep],
                  by = "subject_id", all.x = TRUE)
  cohort[, sex := factor(gender, levels = c("F","M"))]
  cohort[, gender := NULL]

  cohort[, age_years  := as.numeric(difftime(admittime, dob,
                                             units = "days")) / 365.25]
  cohort[, age_months := as.integer(floor(
    as.numeric(difftime(admittime, dob, units = "days")) / 30.4375
  ))]

  before <- nrow(cohort)
  cohort <- cohort[age_years >= 0 & age_years <= 18]
  steps <- log_step(steps, "age_0_to_18", nrow(cohort),
                    excluded = before - nrow(cohort),
                    reason = "age outside [0, 18] y at admission")

  before <- nrow(cohort)
  cohort <- cohort[!is.na(sex)]
  steps <- log_step(steps, "known_sex", nrow(cohort),
                    excluded = before - nrow(cohort),
                    reason = "sex missing or non-{F,M}")

  cohort[, los_hours := as.numeric(difftime(outtime, intime, units = "hours"))]

  before <- nrow(cohort)
  cohort <- cohort[los_hours >= min_los_hours]
  steps <- log_step(steps, sprintf("los_ge_%dh", as.integer(min_los_hours)),
                    nrow(cohort),
                    excluded = before - nrow(cohort),
                    reason = sprintf("LOS < %d h", as.integer(min_los_hours)))

  cohort[, hospital_expire_flag := as.integer(hospital_expire_flag)]
  if ("deathtime" %in% names(cohort)) {
    n_mismatch <- cohort[, sum(!is.na(deathtime) & hospital_expire_flag == 0L)]
    if (n_mismatch > 0L && isTRUE(verbose)) {
      message(sprintf(
        "[cohort] %-32s n=%6d  (DEATHTIME present but flag=0; flag wins per spec)",
        "deathtime_flag_mismatch", n_mismatch
      ))
    }
  }

  cohort[, admit_year := data.table::year(admittime)]

  ## Window-aware is_surgical: only count SURGERY_VITAL_SIGNS evidence
  ## whose `MONITORTIME` is strictly before `T0 + prediction_window_hours`.
  ## A bare `hadm_id %in% surg_hadms` join would leak future evidence
  ## (e.g. surgery in hour 36 marking a patient as surgical at T+24h).
  surg <- data.table::fread(cmd = sprintf("gunzip -c %s",
                                          shQuote(paths$surgery_vital_signs)),
                            select = c("HADM_ID","MONITORTIME"),
                            showProgress = FALSE)
  data.table::setnames(surg, tolower(names(surg)))
  surg[, monitortime := as.POSIXct(monitortime, tz = "UTC")]

  intime_lookup <- cohort[, list(hadm_id, intime)]
  data.table::setkey(intime_lookup, hadm_id)
  surg <- merge(surg, intime_lookup, by = "hadm_id",
                all.x = FALSE, all = FALSE)
  surg[, t_hours := as.numeric(difftime(monitortime, intime,
                                        units = "hours"))]
  surg_in_window <- surg[!is.na(t_hours) & t_hours >= 0 &
                          t_hours < prediction_window_hours]
  surg_hadms_window <- unique(surg_in_window$hadm_id)
  cohort[, is_surgical := hadm_id %in% surg_hadms_window]

  if (isTRUE(verbose)) {
    n_after  <- surg[, sum(t_hours >= prediction_window_hours, na.rm = TRUE)]
    n_window <- nrow(surg_in_window)
    message(sprintf(
      "[cohort] is_surgical (window-aware): %d positive (in window=%d; after window=%d; window=%dh)",
      sum(cohort$is_surgical), n_window, n_after,
      as.integer(prediction_window_hours)
    ))
  }

  primary_icd <- if ("icd10_code_cn" %in% names(cohort)) {
    cohort$icd10_code_cn
  } else {
    NA_character_
  }
  cohort[, primary_icd_chapter := factor(icd10_to_chapter(primary_icd))]

  out <- cohort[, list(subject_id, hadm_id, icustay_id,
                       intime, outtime, los_hours,
                       age_months, age_years, sex,
                       hospital_expire_flag, admit_year,
                       is_surgical, primary_icd_chapter)]

  attrition <- data.table::rbindlist(steps, fill = TRUE)
  data.table::setattr(out, "attrition", attrition)
  data.table::setattr(out, "min_los_hours", as.integer(min_los_hours))
  out[]
}

#' Cohort attrition table (for the manuscript Methods figure)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Returns the exclusion cascade documenting how many ICU stays were
#' dropped at each cohort-filter step. Surfaces the same data carried
#' as the `"attrition"` attribute on [build_cohort()]'s output.
#'
#' @param paths Output of [pic_paths()].
#' @param min_los_hours See [build_cohort()].
#'
#' @return A `data.table` of (step, n, excluded, reason).
#'
#' @examples
#' \dontrun{
#' paths <- pic_paths()
#' attrition <- cohort_attrition(paths, min_los_hours = 24L)
#' print(attrition)
#' }
#' @export
cohort_attrition <- function(paths, min_los_hours = 24L) {
  cohort <- build_cohort(paths, min_los_hours = min_los_hours, verbose = FALSE)
  attr(cohort, "attrition")
}

#' Cohort invariants (used by tests and as a sanity gate at runtime)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Verifies that a cohort `data.table` satisfies the structural and
#' epidemiological invariants committed in `vignettes/cohort_spec.Rmd`.
#' Raises a structured error listing every violation; passes silently
#' (invisibly returning `TRUE`) when all invariants hold.
#'
#' @param cohort A cohort `data.table` from [build_cohort()].
#' @param expected Named list of expected ranges. Defaults to the values
#'   committed in `vignettes/cohort_spec.Rmd`.
#'
#' @return Invisibly returns `TRUE` if all invariants hold; raises an
#'   error listing every violation otherwise.
#'
#' @examples
#' toy <- readRDS(system.file("extdata", "toy_cohort.rds",
#'                            package = "picMort"))
#' toy_expectations <- list(
#'   n_min            = 50L,
#'   n_max            = 150L,
#'   mortality_rate   = c(0.05, 0.20),
#'   age_range_years  = c(0, 18),
#'   sex_levels       = c("F", "M"),
#'   distinct_subject = TRUE,
#'   no_overlap_stays = TRUE
#' )
#' assert_cohort_invariants(toy, expected = toy_expectations)
#' @export
assert_cohort_invariants <- function(cohort,
                                     expected = default_cohort_expectations()) {
  v <- character(0)
  n <- nrow(cohort)
  if (n < expected$n_min) {
    v <- c(v, sprintf("n=%d below n_min=%d", n, expected$n_min))
  }
  if (n > expected$n_max) {
    v <- c(v, sprintf("n=%d above n_max=%d", n, expected$n_max))
  }

  rate <- mean(cohort$hospital_expire_flag, na.rm = TRUE)
  if (rate < expected$mortality_rate[1]) {
    v <- c(v, sprintf("mortality=%.4f below lower=%.4f",
                      rate, expected$mortality_rate[1]))
  }
  if (rate > expected$mortality_rate[2]) {
    v <- c(v, sprintf("mortality=%.4f above upper=%.4f",
                      rate, expected$mortality_rate[2]))
  }

  age_rng <- range(cohort$age_years, na.rm = TRUE)
  if (age_rng[1] < expected$age_range_years[1] - 1e-6 ||
      age_rng[2] > expected$age_range_years[2] + 1e-6) {
    v <- c(v, sprintf("age range [%.3f, %.3f] outside expected [%g, %g]",
                      age_rng[1], age_rng[2],
                      expected$age_range_years[1], expected$age_range_years[2]))
  }

  if (!setequal(levels(cohort$sex), expected$sex_levels)) {
    v <- c(v, sprintf("sex levels {%s} != expected {%s}",
                      paste(levels(cohort$sex), collapse = ","),
                      paste(expected$sex_levels, collapse = ",")))
  }

  if (isTRUE(expected$distinct_subject) &&
      data.table::uniqueN(cohort$subject_id) != nrow(cohort)) {
    v <- c(v, paste0("first-stay-per-patient invariant violated ",
                     "(duplicate subject_id)"))
  }

  if (length(v) > 0L) {
    stop("Cohort invariants failed:\n - ",
         paste(v, collapse = "\n - "), call. = FALSE)
  }
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
default_cohort_expectations <- function() {
  # Locked against PIC v1.1.0 + min_los_hours = 24L (committed 2026-05-08).
  # Observed at G1: n = 8,736; mortality = 0.0844 (737 / 8,736 deaths);
  # age range [0, 17.842] y; admit-year span 2060-2118; sex F/M = 3,684 / 5,052.
  # Bands chosen to catch silent build-code changes, not natural cohort drift.
  list(
    n_min            = 8500L,
    n_max            = 9000L,
    mortality_rate   = c(0.075, 0.095),
    age_range_years  = c(0, 18),
    sex_levels       = c("F", "M"),
    distinct_subject = TRUE,
    no_overlap_stays = TRUE
  )
}
