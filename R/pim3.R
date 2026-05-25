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
##   Straney L, Clements A, Parslow RC, et al. Pediatric Index of
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
#' Coefficients are the **published PIM3 values** verbatim from
#' Straney L et al. (2013) and the ANZICS PIM2 & PIM3 information booklet
#' (Jan 2019). The linear-predictor expression is, exactly:
#'
#'   PIM3val = (3.8233 * Pupils) − (0.5378 * Elective) + (0.9763 * MechVent)
#'           + (0.0671 * |Base Excess|) − (0.0431 * SBP)
#'           + (0.1716 * (SBP² / 1000)) + (0.4214 * (100*FiO2/PaO2))
#'           − (1.2246 * Recov_CardBypPr) − (0.8762 * Recov_CardNonBypPr)
#'           − (1.5164 * Recov_NonCardPr) + (1.6225 * VHRdiag)
#'           + (1.0725 * HRdiag) − (2.1766 * LRdiag) − 1.7928
#'
#' Missing-value defaults (PIM3-specific, differ from PIM2): SBP → 120;
#' (100*FiO2/PaO2) → 0.23 (room-air normal, not 0 as in PIM2); Base
#' Excess → 0; all binary fields → 0. Only one of VHRdiag/HRdiag/LRdiag
#' may be 1 at a time (highest-applicable-risk wins).
#'
#' @keywords internal
#' @noRd
pim3_coefficients <- function() {
  list(
    intercept             = -1.7928,
    pupils_fixed          =  3.8233,
    elective              = -0.5378,
    mech_vent             =  0.9763,
    base_excess_abs       =  0.0671,
    sbp_linear            = -0.0431,
    sbp_squared_over_1000 =  0.1716,
    fio2_pao2             =  0.4214,    # × (100 * FiO2_fraction / PaO2_mmHg)
    recov_card_byp        = -1.2246,    # recovery from cardiac bypass procedure
    recov_card_nonbyp     = -0.8762,    # recovery from non-bypass cardiac procedure
    recov_noncard         = -1.5164,    # recovery from non-cardiac procedure
    very_high_risk_dx     =  1.6225,
    high_risk_dx          =  1.0725,
    low_risk_dx           = -2.1766
  )
}

#' Default values for PIM3 missing-component handling
#'
#' Per the ANZICS PIM2 & PIM3 booklet (Jan 2019), PIM3 — unlike PIM2 —
#' uses non-zero defaults for two components: SBP → 120 mmHg, and
#' (100 * FiO2 / PaO2) → 0.23 (room-air "normal" substitute, derived
#' from PaO2 in air ≈ (0.21*100)/90). All other components default to 0.
#'
#' @keywords internal
#' @noRd
pim3_defaults <- function() {
  list(
    sbp        = 120,
    fio2_pao2  = 0.23,
    base_excess = 0,
    mech_vent  = 0L,
    elective   = 0L,
    pupils_fixed = 0L,
    recov_card_byp    = 0L,
    recov_card_nonbyp = 0L,
    recov_noncard     = 0L
  )
}

#' PIM3 risk-group ICD-10 mapping (pediatric, simplified)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Maps an ICD-10 code to one of `"low"`, `"high"`, `"very_high"`,
#' or `NA_character_` (no risk-group assignment). Covers the most
#' frequent pediatric admissions per Straney 2013 Tables 2-4. Codes
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

  # PIM3 LRdiag (Straney 2013 / ANZICS booklet PIM3 §7):
  # asthma, bronchiolitis, croup, OSA, DKA, seizures.
  is_low <- base3 %in% c("J45","J46",          # asthma
                          "J21",                # bronchiolitis
                          "J05",                # croup
                          "G40", "R56",         # seizures
                          "G473")               # OSA (G47.3 prefix-3)
  is_low <- is_low | grepl("^E1[0-4]\\.?1", cu) # DKA E10.1/E11.1/E13.1/E14.1

  # PIM3 HRdiag (ANZICS booklet PIM3 §6):
  # spontaneous cerebral haemorrhage, cardiomyopathy/myocarditis, HLHS,
  # neurodegenerative disorder, necrotising enterocolitis.
  # Septic shock is collected as HRdiag in the registry but is NOT used
  # in the PIM3 calculation, so we omit it here.
  is_high <- base3 %in% c("I42",               # cardiomyopathy
                           "I40", "I41",        # myocarditis
                           "G31", "G37",        # neurodegenerative disorder
                           "I60", "I61", "I62", # spontaneous cerebral haemorrhage
                           "P77",               # necrotising enterocolitis (perinatal NEC)
                           "K55")               # necrotising enterocolitis (vascular intestinal)
  is_high <- is_high | grepl("^Q23\\.?4", cu)   # HLHS Q23.4

  # PIM3 VHRdiag (ANZICS booklet PIM3 §5):
  # cardiac arrest preceding ICU admission, SCID, leukaemia/lymphoma after
  # 1st induction, BMT recipient, liver failure (NOT post-liver-transplant),
  # SCID+BMT, leukaemia+BMT. Necrotising enterocolitis (code 6) is no
  # longer in VHRdiag in PIM3 and is now coded as HRdiag — already handled
  # above.
  is_very <- base3 %in% c("I46",               # cardiac arrest preceding ICU
                           "D81",               # SCID (very-high in PIM3, not high)
                           "C81", "C82", "C83", "C84", "C85", # lymphomas
                           "C91", "C92", "C93", "C94", "C95", # leukaemias
                           "K72")               # liver failure (acute/chronic)
  is_very <- is_very | grepl("^Z94\\.?(8|81|84)", cu)  # post-BMT

  # Highest-applicable-risk wins (PIM3 §11 Q&A: "while a patient can
  # have a low, high, and very high risk score, only the highest risk
  # score is used in the PIM3 calculation").
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
#' Computes the Pediatric Index of Mortality 3 (Straney et al. 2013)
#' from PIC `CHARTEVENTS` and the `cohort` table. Components that
#' cannot be recovered from PIC default to 0 (Straney convention) and
#' are listed in `proxy_flags`.
#'
#' Demonstrating the linear-predictor calculation without PIC source files
#' (this is what `compute_pim3()` produces internally per patient).
#' This is the ANZICS PIM2 & PIM3 booklet's worked example (Jan 2019,
#' p. 11): a 6 y-old girl, leukaemia post-1st-induction, intubated, SBP
#' 70 mmHg, PaO2 65 mmHg, FiO2 0.7, base excess −12 mmol/L, reactive
#' pupils, non-elective. The booklet gives PIM3val = −0.11114 and risk
#' of death = 47.22%.
#'
#' ```r
#' beta <- picMort:::pim3_coefficients()
#' logit <-
#'   beta$intercept +                       # -1.7928
#'   beta$pupils_fixed          * 0 +        # reactive pupils
#'   beta$elective              * 0 +        # non-elective
#'   beta$mech_vent             * 1 +        # intubated
#'   beta$base_excess_abs       * 12 +       # |-12|
#'   beta$sbp_linear            * 70 +       # SBP 70 mmHg
#'   beta$sbp_squared_over_1000 * (70 * 70 / 1000) +
#'   beta$fio2_pao2             * (100 * 0.7 / 65) +  # 100*FiO2/PaO2
#'   beta$recov_card_byp        * 0 +
#'   beta$recov_card_nonbyp     * 0 +
#'   beta$recov_noncard         * 0 +
#'   beta$very_high_risk_dx     * 1          # leukaemia after 1st induction
#' stats::plogis(logit)  # ≈ 0.4722
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
#' @references Straney L et al. (2013). PIM3: an updated Pediatric
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
  ce <- ce[t_hours >= 0 & t_hours < window_hours &
           !is.na(valuenum) & valuenum > 0]

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
  ## PIC v1.1.0 does not distinguish cardiac vs non-cardiac surgery
  ## cleanly, nor bypass vs non-bypass cardiac procedures. The
  ## `is_surgical` flag (any SURGERY_VITAL_SIGNS row within the first-24h
  ## window per the window-aware cohort spec) is mapped to PIM3's
  ## `Recov_NonCardPr` — the most clinically common recovery case in a
  ## general pediatric ICU. The two cardiac-procedure indicators
  ## (`Recov_CardBypPr`, `Recov_CardNonBypPr`) default to 0 with a
  ## documented proxy flag.
  recovery_dt <- cohort[, list(icustay_id,
                               recov_card_byp    = 0L,
                               recov_card_nonbyp = 0L,
                               recov_noncard     = as.integer(is_surgical))]

  ## --- Assemble component table ---------------------------------------------
  pim3 <- merge(cohort[, list(icustay_id)], sbp_tbl,
                by = "icustay_id", all.x = TRUE)
  pim3 <- merge(pim3, risk_dt,     by = "icustay_id", all.x = TRUE)
  pim3 <- merge(pim3, recovery_dt, by = "icustay_id", all.x = TRUE)

  ## Default unrecovered components (PIM3-specific; see pim3_defaults()).
  ## PIM3 differs from PIM2 on SBP and FiO2/PaO2 defaults.
  defaults <- pim3_defaults()
  pim3[, sbp_used    := ifelse(is.na(sbp_first_hour_min),
                               defaults$sbp, sbp_first_hour_min)]
  pim3[, fio2_pao2    := defaults$fio2_pao2]  # 0.23 (room-air normal), NOT 0
  pim3[, base_excess  := defaults$base_excess]
  pim3[, mech_vent    := defaults$mech_vent]
  pim3[, elective     := defaults$elective]
  pim3[, pupils_fixed := defaults$pupils_fixed]

  ## Proxy flags per row.
  flag_each <- function(sbp_first, rg) {
    f <- c("fio2_pao2_default_0.23",   # PIC has no first-hour ABG → default
           "base_excess",
           "mech_vent",
           "elective",
           "pupils_fixed",
           "cardiac_bypass_status")     # 3-way recovery split unrecoverable
    if (is.na(sbp_first)) f <- c(f, "sbp")
    if (rg == "default")  f <- c(f, "risk_group_default")
    f
  }
  pim3[, proxy_flags := mapply(flag_each, sbp_first_hour_min, risk_group,
                               SIMPLIFY = FALSE)]

  ## --- Linear predictor (Straney 2013 / ANZICS Jan 2019 booklet) -------------
  pim3[, pim3_logit :=
         beta$intercept +
         beta$pupils_fixed          * pupils_fixed +
         beta$elective              * elective +
         beta$mech_vent             * mech_vent +
         beta$base_excess_abs       * abs(base_excess) +
         beta$sbp_linear            * sbp_used +
         beta$sbp_squared_over_1000 * (sbp_used * sbp_used / 1000) +
         beta$fio2_pao2             * fio2_pao2 +
         beta$recov_card_byp        * recov_card_byp +
         beta$recov_card_nonbyp     * recov_card_nonbyp +
         beta$recov_noncard         * recov_noncard +
         ifelse(risk_group == "very_high", beta$very_high_risk_dx, 0) +
         ifelse(risk_group == "high",      beta$high_risk_dx,      0) +
         ifelse(risk_group == "low",       beta$low_risk_dx,       0)]

  pim3[, pim3 := stats::plogis(pim3_logit)]

  pim3[, list(icustay_id, pim3_logit, pim3,
              risk_group = factor(
                risk_group,
                levels = c("default", "low", "high", "very_high")
              ),
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
