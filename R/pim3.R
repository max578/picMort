## ============================================================================
## Gate G3 -- PIM3 reconstruction from PIC (Straney et al. 2013)
##
## PIM3 is computed from variables collected in the first hour of ICU
## care. PIC v1.1.0 exposes a subset of the required components cleanly:
## SBP from CHARTEVENTS (ITEMID 1016), surgical-recovery proxy from
## SURGERY_VITAL_SIGNS, and diagnostic risk groups via ICD-10 mapping.
## Components requiring data PIC does not expose (pupillary reactions,
## FiO2/PaO2 ratio derivable only via paired ABG values, base excess
## from arterial gas, mechanical-ventilation flag at hour 1, elective
## vs emergency status, cardiac-bypass flag) default to 0 per Straney's
## published convention for unrecoverable components, with a
## `proxy_flags` list-column marking which entries used a default.
##
## Reference:
##   Straney L, Clements A, Parslow RC, et al. Paediatric Index of
##   Mortality 3: An Updated Model for Predicting Mortality in
##   Pediatric Intensive Care. Pediatr Crit Care Med. 2013;14:673-681.
##
## The face-validity check (`pim3_face_validity()`) reports the
## observed-vs-expected ratio against the cohort's actual
## hospital-mortality rate. A wildly off O/E indicates either (a) too
## many missing components for PIM3 to discriminate or (b) a coding
## error -- both surface in the manuscript Methods + Limitations.
## ============================================================================

#' PIM3 coefficient table (Straney et al. 2013)
#'
#' @keywords internal
#' @noRd
pim3_coefficients <- function() {
  list(
    intercept              = -0.6234,
    sbp_linear             = -0.0431,    # × SBP (mmHg)
    sbp_abs_dev_120        =  0.01395,   # × |SBP - 120|
    fio2_pao2              =  0.4214,    # × (FiO2*100 / PaO2)
    base_excess_abs        =  0.1667,    # × |base excess|
    mech_vent              =  1.3352,
    elective               =  0.9763,
    recovery_no_bypass     =  1.6225,
    recovery_bypass        =  1.0725,
    low_risk_dx            = -1.5770,
    high_risk_dx           =  1.0044,
    very_high_risk_dx      =  2.4451,
    pupils_fixed           = -1.2018     # encoded -1.2018 reflects sign in Straney
  )
}

#' PIM3 risk-group ICD-10 mapping (paediatric, simplified)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Maps an ICD-10 code to one of `"low"`, `"high"`, `"very_high"`,
#' or `NA_character_` (no risk-group assignment). Covers the most
#' frequent paediatric admissions per Straney 2013 Tables 2-4. Codes
#' not in the lookup return `NA`, treated as default-risk for PIM3.
#'
#' @param code Character vector of ICD-10 codes.
#' @return Character vector of risk-group labels (or `NA`).
#'
#' @examples
#' pim3_risk_group(c("J45.0",   # asthma -> low
#'                   "I42.1",   # cardiomyopathy -> high
#'                   "I46",     # post-cardiac-arrest -> very_high
#'                   "K52.901", # unmapped -> NA
#'                   NA_character_))
#' @export
pim3_risk_group <- function(code) {
  out <- rep(NA_character_, length(code))
  ok  <- !is.na(code) & nzchar(code)
  if (!any(ok)) return(out)
  cu  <- toupper(code[ok])
  base3 <- substr(cu, 1L, 3L)

  is_low <- base3 %in% c("J45","J46",          # asthma
                          "J21",                # bronchiolitis
                          "J05",                # croup
                          "G40", "R56",         # seizure
                          "G473")               # OSA (G47.3 prefix-3)
  is_low <- is_low | grepl("^E1[0-4]\\.?1", cu) # DKA E10.1/E11.1/E13.1/E14.1

  is_high <- base3 %in% c("I42",               # cardiomyopathy
                           "I40", "I41",        # myocarditis
                           "G31", "G37",        # neurodegenerative
                           "D81")               # SCID
  is_high <- is_high | grepl("^Q23\\.?4", cu)   # HLHS Q23.4

  is_very <- base3 %in% c("I46",               # post-cardiac-arrest
                           "K72",               # liver failure
                           "C91", "C92", "C95") # leukaemia
  is_very <- is_very | grepl("^Z94\\.?(8|81|84)", cu)  # post-BMT

  out[ok][is_very] <- "very_high"
  out[ok][is_high & is.na(out[ok])] <- "high"
  out[ok][is_low  & is.na(out[ok])] <- "low"
  out
}

#' Reconstruct PIM3 from PIC fields
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Computes the Paediatric Index of Mortality 3 (Straney et al. 2013)
#' from PIC `CHARTEVENTS` and the `cohort` table. Components that
#' cannot be recovered from PIC default to 0 (Straney convention) and
#' are listed in `proxy_flags`.
#'
#' Demonstrating the linear-predictor calculation without PIC source files
#' (this is what `compute_pim3()` produces internally per patient):
#'
#' ```r
#' beta <- picMort:::pim3_coefficients()
#' sbp_used <- 120
#' logit <- beta$intercept +
#'          beta$sbp_linear * sbp_used +
#'          beta$sbp_abs_dev_120 * abs(sbp_used - 120)
#' stats::plogis(logit) # baseline PIM3 risk
#' ```
#'
#' @param cohort A cohort `data.table` from [build_cohort()].
#' @param paths Output of [pic_paths()].
#' @param window_hours Time window in hours for first-hour PIM3
#'   components. Straney uses 1 hour; we follow.
#'
#' @return A `data.table` keyed on `icustay_id` with columns:
#'   `pim3_logit` (numeric), `pim3` (probability), `risk_group`
#'   (factor: low / default / high / very_high), `sbp_used` (numeric),
#'   `proxy_flags` (list-column; vector of components that defaulted).
#'
#' @references Straney L et al. (2013). PIM3: an updated Paediatric
#'   Index of Mortality. Pediatr Crit Care Med 14(7):673-681.
#'
#' @examples
#' \dontrun{
#' paths    <- pic_paths()
#' cohort   <- build_cohort(paths, min_los_hours = 24L, verbose = FALSE)
#' pim3_tbl <- compute_pim3(cohort, paths)
#' summary(pim3_tbl$pim3)
#' table(pim3_tbl$risk_group)
#' }
#' @export
compute_pim3 <- function(cohort, paths, window_hours = 1L) {
  beta <- pim3_coefficients()

  ## --- SBP from CHARTEVENTS in first `window_hours` --------------------------
  ## Use lowest SBP in window (clinical convention: worst value).
  ce <- data.table::fread(
    paths$chartevents,
    select = c("HADM_ID","ICUSTAY_ID","ITEMID","CHARTTIME","VALUENUM"),
    showProgress = FALSE
  )
  data.table::setnames(ce, tolower(names(ce)))
  ce <- ce[itemid == 1016L]              # SBP only
  ce <- ce[hadm_id %in% cohort$hadm_id]
  ce <- merge(ce,
              cohort[, list(hadm_id,
                            icustay_id_first = icustay_id,
                            intime_first     = intime)],
              by = "hadm_id", all.x = TRUE, allow.cartesian = TRUE)
  ce <- ce[is.na(icustay_id) | icustay_id == icustay_id_first]
  ce[, icustay_id := icustay_id_first]
  ce[, intime     := intime_first]
  ce[, c("icustay_id_first","intime_first") := NULL]
  ce[, charttime := as.POSIXct(charttime, tz = "UTC")]
  ce[, t_hours   := as.numeric(difftime(charttime, intime, units = "hours"))]
  ce <- ce[t_hours >= 0 & t_hours < window_hours & !is.na(valuenum) & valuenum > 0]

  sbp_tbl <- ce[, list(sbp_first_hour_min = min(valuenum)), by = icustay_id]

  ## --- Risk groups from primary ICD (re-read from ADMISSIONS) ----------------
  ## `build_cohort()` drops the raw `icd10_code_cn` (it's preserved only as
  ## chapter); PIM3 needs the full 3+ character code. Use highest-risk-wins:
  ## (very_high > high > low). When a patient has multiple diagnoses in
  ## DIAGNOSES_ICD, we additionally scan all of them and apply the same rule.
  adm <- data.table::fread(paths$admissions,
                           select = c("HADM_ID","ICD10_CODE_CN"),
                           showProgress = FALSE)
  data.table::setnames(adm, tolower(names(adm)))
  dx_long <- data.table::fread(paths$diagnoses_icd,
                               select = c("HADM_ID","ICD10_CODE_CN"),
                               showProgress = FALSE)
  data.table::setnames(dx_long, tolower(names(dx_long)))
  dx_all <- rbind(adm, dx_long, fill = TRUE)
  dx_all <- dx_all[hadm_id %in% cohort$hadm_id]
  dx_all[, risk_group := pim3_risk_group(icd10_code_cn)]

  risk_priority <- c("very_high" = 3L, "high" = 2L, "low" = 1L)
  dx_all[, prio := risk_priority[risk_group]]
  dx_top <- dx_all[!is.na(prio), list(prio = max(prio)), by = hadm_id]
  dx_top[, risk_group := names(risk_priority)[match(prio, risk_priority)]]
  risk_dt <- merge(cohort[, list(icustay_id, hadm_id)],
                   dx_top[, list(hadm_id, risk_group)],
                   by = "hadm_id", all.x = TRUE)
  risk_dt[is.na(risk_group), risk_group := "default"]
  risk_dt <- risk_dt[, list(icustay_id, risk_group)]

  ## --- Recovery proxy: surgical flag from cohort -----------------------------
  ## We use cohort$is_surgical as a proxy for "recovery from procedure".
  ## Cardiac-bypass flag is unrecoverable from PIC fields; default 0.
  recovery_dt <- cohort[, list(icustay_id,
                               recovery_no_bypass = as.integer(is_surgical),
                               recovery_bypass    = 0L)]

  ## --- Assemble component table ---------------------------------------------
  pim3 <- merge(cohort[, list(icustay_id)], sbp_tbl,    by = "icustay_id", all.x = TRUE)
  pim3 <- merge(pim3, risk_dt,     by = "icustay_id", all.x = TRUE)
  pim3 <- merge(pim3, recovery_dt, by = "icustay_id", all.x = TRUE)

  ## Default unrecovered components (Straney convention).
  pim3[, sbp_used     := ifelse(is.na(sbp_first_hour_min), 120, sbp_first_hour_min)]
  pim3[, fio2_pao2    := 0]
  pim3[, base_excess  := 0]
  pim3[, mech_vent    := 0L]
  pim3[, elective     := 0L]
  pim3[, pupils_fixed := 0L]

  ## Proxy flags per row.
  flag_each <- function(sbp_first, rg) {
    f <- c("fio2_pao2", "base_excess", "mech_vent", "elective",
           "pupils_fixed", "cardiac_bypass")
    if (is.na(sbp_first)) f <- c(f, "sbp")
    if (rg == "default")  f <- c(f, "risk_group_default")
    f
  }
  pim3[, proxy_flags := mapply(flag_each, sbp_first_hour_min, risk_group,
                               SIMPLIFY = FALSE)]

  ## --- Linear predictor ------------------------------------------------------
  pim3[, pim3_logit :=
         beta$intercept +
         beta$sbp_linear      * sbp_used +
         beta$sbp_abs_dev_120 * abs(sbp_used - 120) +
         beta$fio2_pao2       * fio2_pao2 +
         beta$base_excess_abs * abs(base_excess) +
         beta$mech_vent       * mech_vent +
         beta$elective        * elective +
         beta$recovery_no_bypass * recovery_no_bypass +
         beta$recovery_bypass    * recovery_bypass +
         ifelse(risk_group == "low",       beta$low_risk_dx, 0) +
         ifelse(risk_group == "high",      beta$high_risk_dx, 0) +
         ifelse(risk_group == "very_high", beta$very_high_risk_dx, 0) +
         beta$pupils_fixed    * pupils_fixed]

  pim3[, pim3 := stats::plogis(pim3_logit)]

  pim3[, list(icustay_id, pim3_logit, pim3,
              risk_group = factor(risk_group,
                                  levels = c("default","low","high","very_high")),
              sbp_used, proxy_flags)]
}

#' PIM3 face-validity check
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Reports the distribution of reconstructed PIM3 probabilities, the
#' observed-vs-expected (O/E) ratio against actual cohort mortality,
#' and a summary of which components defaulted to proxy values. The
#' result is informational, not a hard gate -- imperfect PIM3
#' reconstruction is normal when all components are not recoverable
#' from the source database; the manuscript Limitations records
#' which components were proxied.
#'
#' @param pim3_tbl Output of [compute_pim3()].
#' @param cohort Cohort `data.table` with `hospital_expire_flag`.
#'
#' @return A list with elements `summary` (named numeric of pim3
#'   distribution stats), `oe_ratio`, `oe_ci` (Wilson 95% CI),
#'   `proxy_freq` (table of which proxies fired and how often),
#'   `risk_group_counts` (table), `notes` (character).
#'
#' @examples
#' toy <- readRDS(system.file("extdata", "toy_cohort.rds",
#'                            package = "picMort"))
#' # Build a minimal synthetic pim3 table matching compute_pim3()'s schema.
#' set.seed(20260517L)
#' pim3_tbl <- data.table::data.table(
#'   icustay_id  = toy$icustay_id,
#'   pim3_logit  = stats::rnorm(nrow(toy), mean = -2.5, sd = 0.5),
#'   pim3        = stats::plogis(stats::rnorm(nrow(toy), -2.5, 0.5)),
#'   risk_group  = factor(sample(c("default", "low", "high", "very_high"),
#'                               nrow(toy), replace = TRUE,
#'                               prob = c(0.7, 0.2, 0.08, 0.02)),
#'                        levels = c("default", "low", "high", "very_high")),
#'   sbp_used    = stats::rnorm(nrow(toy), 110, 20),
#'   proxy_flags = replicate(nrow(toy),
#'                           c("fio2_pao2", "base_excess"), simplify = FALSE)
#' )
#' fv <- pim3_face_validity(pim3_tbl, toy)
#' fv$summary
#' fv$oe_ratio
#' @export
pim3_face_validity <- function(pim3_tbl, cohort) {
  joined <- merge(pim3_tbl, cohort[, list(icustay_id, hospital_expire_flag)],
                  by = "icustay_id", all.x = TRUE)
  obs <- mean(joined$hospital_expire_flag, na.rm = TRUE)
  exp <- mean(joined$pim3, na.rm = TRUE)
  oe  <- obs / exp
  n_obs <- sum(joined$hospital_expire_flag, na.rm = TRUE)
  n_total <- nrow(joined)
  wilson <- stats::prop.test(n_obs, n_total)$conf.int

  proxy_freq <- table(unlist(joined$proxy_flags))

  list(
    summary = c(n     = nrow(joined),
                pim3_mean   = exp,
                pim3_median = stats::median(joined$pim3, na.rm = TRUE),
                pim3_q25    = stats::quantile(joined$pim3, 0.25, na.rm = TRUE),
                pim3_q75    = stats::quantile(joined$pim3, 0.75, na.rm = TRUE),
                obs_mortality = obs),
    oe_ratio  = oe,
    oe_ci     = c(lower = wilson[1] / exp, upper = wilson[2] / exp),
    proxy_freq = proxy_freq,
    risk_group_counts = table(joined$risk_group),
    notes = paste0(
      "Components defaulted to 0 (Straney convention) for the following ",
      "always-missing variables in PIC: pupils_fixed, base_excess, ",
      "fio2_pao2, mech_vent (no first-hour ventilation flag), elective ",
      "(no clean elective/emergency variable), cardiac_bypass. SBP and ",
      "surgical-recovery proxy and ICD-10 risk groups are recovered from ",
      "PIC. Imperfect O/E is expected; manuscript Limitations records the ",
      "proxied components."
    )
  )
}
