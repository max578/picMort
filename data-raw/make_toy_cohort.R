# =============================================================================
# data-raw/make_toy_cohort.R
#
# Generate a deterministic synthetic cohort that matches the schema produced
# by `build_cohort()` and ships in `inst/extdata/toy_cohort.rds`. The toy
# cohort underpins all `@examples` for data-aware exported functions and
# unlocks 8 of 9 currently-skipped tests.
#
# This file is excluded from the package build via `.Rbuildignore`.
#
# Run with:
#   Rscript data-raw/make_toy_cohort.R
# or from R:
#   source("data-raw/make_toy_cohort.R")
# =============================================================================

set.seed(20260517L)

library(data.table)

n <- 80L
mortality_rate <- 0.10  # 8 / 80 = 10 % — close to PIC's ~8.4 %

# Stable identifiers
subject_id <- seq_len(n) + 100000L
hadm_id    <- subject_id + 500000L
icustay_id <- subject_id + 900000L

# Age distribution: mix of infants, toddlers, school-age, adolescents
age_years_raw <- c(
  runif(20, 0.01, 1.0),
  runif(20, 1.0, 5.0),
  runif(20, 5.0, 12.0),
  runif(20, 12.0, 18.0)
)
age_years  <- sample(age_years_raw)  # shuffle so age is not sorted
age_months <- as.integer(floor(age_years * 12))

# Sex
sex <- factor(sample(c("F", "M"), n, replace = TRUE, prob = c(0.45, 0.55)),
              levels = c("F", "M"))

# Admit times spread across PIC's 2060-2118 window
admit_year <- sample(seq(2060L, 2118L), n, replace = TRUE)
intime <- as.POSIXct(
  paste0(admit_year, "-",
         sprintf("%02d", sample(1:12, n, replace = TRUE)), "-",
         sprintf("%02d", sample(1:28, n, replace = TRUE)), " ",
         sprintf("%02d", sample(0:23, n, replace = TRUE)), ":00:00"),
  tz = "UTC"
)

# ICU LOS: all >= 24 hours (cohort filter). Distribution from a log-normal
# truncated at 24 h so most stays are 1-7 days.
los_hours <- pmax(24, round(rlnorm(n, meanlog = 4.4, sdlog = 0.8), 1))
outtime <- intime + los_hours * 3600

# Surgical flag: ~ 40 %
is_surgical <- as.logical(rbinom(n, 1L, prob = 0.4))

# Primary ICD chapter: spread across paediatric-relevant chapters so that
# `subgroup_performance()` and `icd10_to_chapter()` round-trip and so PIM3
# risk-group mapping has at least one entry per category.
icd_chapter_levels <- c(
  "respiratory", "infectious", "circulatory", "perinatal",
  "neoplasms",   "nervous",    "congenital",  "digestive",
  "endocrine",   "injury",     "blood",       "symptoms"
)
primary_icd_chapter <- factor(
  sample(icd_chapter_levels, n, replace = TRUE,
         prob = c(0.18, 0.13, 0.12, 0.10,
                  0.08, 0.08, 0.07, 0.06,
                  0.06, 0.05, 0.04, 0.03)),
  levels = icd_chapter_levels
)

# Outcome: biased so that respiratory + circulatory + neoplasms patients
# are slightly more likely to die — produces a learnable signal for the
# fit_* examples without forcing a particular probability.
risk_score <- 0.08 +
  0.05 * (primary_icd_chapter %in% c("respiratory", "circulatory", "neoplasms")) +
  0.03 * (age_years < 1) +
  0.02 * is_surgical
hospital_expire_flag <- rbinom(n, 1L, prob = pmin(0.35, risk_score))

# Force the realised mortality rate to be exactly 8 / 80 = 0.10 so the
# toy cohort is reproducible bit-for-bit (independent of rbinom roll).
target_deaths <- 8L
current_deaths <- sum(hospital_expire_flag)
if (current_deaths > target_deaths) {
  to_flip <- sample(which(hospital_expire_flag == 1L),
                    current_deaths - target_deaths)
  hospital_expire_flag[to_flip] <- 0L
} else if (current_deaths < target_deaths) {
  to_flip <- sample(which(hospital_expire_flag == 0L),
                    target_deaths - current_deaths)
  hospital_expire_flag[to_flip] <- 1L
}

toy_cohort <- data.table::data.table(
  subject_id           = subject_id,
  hadm_id              = hadm_id,
  icustay_id           = icustay_id,
  intime               = intime,
  outtime              = outtime,
  los_hours            = los_hours,
  age_months           = age_months,
  age_years            = age_years,
  sex                  = sex,
  hospital_expire_flag = as.integer(hospital_expire_flag),
  admit_year           = as.integer(admit_year),
  is_surgical          = is_surgical,
  primary_icd_chapter  = primary_icd_chapter
)

data.table::setkey(toy_cohort, subject_id, hadm_id, icustay_id)

# Sanity check: must satisfy assert_cohort_invariants() under toy bounds
toy_expectations <- list(
  n_min            = 50L,
  n_max            = 150L,
  mortality_rate   = c(0.05, 0.20),
  age_range_years  = c(0, 18),
  sex_levels       = c("F", "M"),
  distinct_subject = TRUE,
  no_overlap_stays = TRUE
)
# Load the package via load_all so assert_cohort_invariants is available
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(quiet = TRUE)
} else {
  source("R/cohort.R")
  source("R/icd_chapter.R")
}
assert_cohort_invariants(toy_cohort, expected = toy_expectations)

dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
saveRDS(toy_cohort,
        file = "inst/extdata/toy_cohort.rds",
        version = 3L)

message(sprintf(
  "[toy_cohort] n=%d, mortality=%.3f (%d / %d), age range [%.2f, %.2f] y, sex F/M=%d/%d",
  nrow(toy_cohort),
  mean(toy_cohort$hospital_expire_flag),
  sum(toy_cohort$hospital_expire_flag),
  nrow(toy_cohort),
  min(toy_cohort$age_years), max(toy_cohort$age_years),
  sum(toy_cohort$sex == "F"), sum(toy_cohort$sex == "M")
))
