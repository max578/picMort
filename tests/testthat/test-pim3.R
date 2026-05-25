test_that("pim3_coefficients returns the published Straney 2013 values", {
  beta <- picMort:::pim3_coefficients()
  expect_equal(beta$intercept,             -1.7928)
  expect_equal(beta$pupils_fixed,           3.8233)
  expect_equal(beta$elective,              -0.5378)
  expect_equal(beta$mech_vent,              0.9763)
  expect_equal(beta$base_excess_abs,        0.0671)
  expect_equal(beta$sbp_linear,            -0.0431)
  expect_equal(beta$sbp_squared_over_1000,  0.1716)
  expect_equal(beta$fio2_pao2,              0.4214)
  expect_equal(beta$recov_card_byp,        -1.2246)
  expect_equal(beta$recov_card_nonbyp,     -0.8762)
  expect_equal(beta$recov_noncard,         -1.5164)
  expect_equal(beta$very_high_risk_dx,      1.6225)
  expect_equal(beta$high_risk_dx,           1.0725)
  expect_equal(beta$low_risk_dx,           -2.1766)
})

test_that("PIM3 reproduces the ANZICS Jan-2019 booklet worked example exactly", {
  # Reference fixture: 6 y-old girl with relapsed leukaemia (very-high risk
  # diagnosis), febrile neutropenia, intubated/ventilated in the first hour,
  # SBP 70 mmHg, PaO2 65 mmHg, FiO2 0.7, base excess −12 mmol/L, reactive
  # pupils, non-elective admission, no surgical recovery. ANZICS booklet
  # (Jan 2019) p. 11 states: PIM3val = -0.11114; risk of death = 47.22%.
  beta <- picMort:::pim3_coefficients()
  logit <-
    beta$intercept +
    beta$pupils_fixed          * 0 +
    beta$elective              * 0 +
    beta$mech_vent             * 1 +
    beta$base_excess_abs       * 12 +
    beta$sbp_linear            * 70 +
    beta$sbp_squared_over_1000 * (70 * 70 / 1000) +
    beta$fio2_pao2             * (100 * 0.7 / 65) +
    beta$recov_card_byp        * 0 +
    beta$recov_card_nonbyp     * 0 +
    beta$recov_noncard         * 0 +
    beta$very_high_risk_dx     * 1 +
    beta$high_risk_dx          * 0 +
    beta$low_risk_dx           * 0
  expect_equal(logit, -0.11114, tolerance = 1e-4)
  expect_equal(stats::plogis(logit), 0.4722, tolerance = 1e-3)
})

test_that("pim3_risk_group classifies known codes correctly per ANZICS Jan-2019", {
  expect_equal(pim3_risk_group("J45.900"), "low")       # asthma → LRdiag
  expect_equal(pim3_risk_group("J21.000"), "low")       # bronchiolitis → LRdiag
  expect_equal(pim3_risk_group("E10.100"), "low")       # DKA T1DM → LRdiag
  expect_equal(pim3_risk_group("G40.900"), "low")       # epilepsy/seizures → LRdiag
  expect_equal(pim3_risk_group("I42.900"), "high")      # cardiomyopathy → HRdiag
  expect_equal(pim3_risk_group("Q23.400"), "high")      # HLHS → HRdiag
  expect_equal(pim3_risk_group("I60.000"), "high")      # spontaneous SAH → HRdiag
  expect_equal(pim3_risk_group("P77.000"), "high")      # NEC (perinatal) → HRdiag
  expect_equal(pim3_risk_group("K55.000"), "high")      # NEC (vascular) → HRdiag
  expect_equal(pim3_risk_group("I46.000"), "very_high") # cardiac arrest → VHRdiag
  expect_equal(pim3_risk_group("K72.000"), "very_high") # liver failure → VHRdiag
  expect_equal(pim3_risk_group("C91.000"), "very_high") # leukaemia → VHRdiag
  expect_equal(pim3_risk_group("C83.000"), "very_high") # lymphoma → VHRdiag
  expect_equal(pim3_risk_group("D81.000"), "very_high") # SCID → VHRdiag (NOT high)
  expect_true(is.na(pim3_risk_group("J18.900")))        # pneumonia → default
  expect_true(is.na(pim3_risk_group(NA_character_)))
})

test_that("compute_pim3 returns a probability per cohort row (gate G3)", {
  pic_root <- file.path(find_project_root(), "data_links", "pic_v110")
  skip_if_not(dir.exists(pic_root), "PIC source not linked at data_links/pic_v110/")

  paths  <- pic_paths()
  cohort <- build_cohort(paths, min_los_hours = 24L, verbose = FALSE)
  p3     <- compute_pim3(cohort, paths, window_hours = 1L)

  expect_equal(nrow(p3), nrow(cohort))
  expect_true(all(p3$icustay_id %in% cohort$icustay_id))
  expect_true(all(p3$pim3 >= 0 & p3$pim3 <= 1))
  expect_setequal(levels(p3$risk_group), c("default","low","high","very_high"))
})

test_that("pim3_face_validity reports an O/E ratio and proxy frequencies", {
  pic_root <- file.path(find_project_root(), "data_links", "pic_v110")
  skip_if_not(dir.exists(pic_root), "PIC source not linked at data_links/pic_v110/")

  paths  <- pic_paths()
  cohort <- build_cohort(paths, min_los_hours = 24L, verbose = FALSE)
  p3     <- compute_pim3(cohort, paths, window_hours = 1L)
  fv     <- pim3_face_validity(p3, cohort)

  expect_named(fv, c("summary","oe_ratio","oe_ci","proxy_freq",
                     "risk_group_counts","notes"))
  expect_true(fv$oe_ratio > 0 && is.finite(fv$oe_ratio))
  expect_true(length(fv$proxy_freq) > 0L)
})
