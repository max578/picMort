test_that("pim3_risk_group classifies known codes correctly", {
  expect_equal(pim3_risk_group("J45.900"), "low")     # asthma
  expect_equal(pim3_risk_group("J21.000"), "low")     # bronchiolitis
  expect_equal(pim3_risk_group("E10.100"), "low")     # DKA T1DM
  expect_equal(pim3_risk_group("I42.900"), "high")    # cardiomyopathy
  expect_equal(pim3_risk_group("Q23.400"), "high")    # HLHS
  expect_equal(pim3_risk_group("I46.000"), "very_high") # cardiac arrest
  expect_equal(pim3_risk_group("K72.000"), "very_high") # liver failure
  expect_equal(pim3_risk_group("C91.000"), "very_high") # leukaemia
  expect_true(is.na(pim3_risk_group("J18.900")))      # pneumonia → default
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
