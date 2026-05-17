test_that("feature dictionary contains no LOS or discharge-time fields (gate G2)", {
  pic_root <- file.path(find_project_root(), "data_links", "pic_v110")
  skip_if_not(dir.exists(pic_root), "PIC source not linked at data_links/pic_v110/")

  paths    <- pic_paths()
  cohort   <- build_cohort(paths, min_los_hours = 24L, verbose = FALSE)
  features <- build_features(cohort, paths, window_hours = 24L,
                             feature_set = "simple")

  forbidden <- c("\\blos\\b", "\\blos_", "_los\\b",
                 "outtime", "discharge", "dischtime", "deathtime")
  hits <- features$dict[grepl(paste(forbidden, collapse = "|"),
                              tolower(variable))]
  expect_equal(nrow(hits), 0L,
               info = "Forbidden temporal fields leaked into features")

  expect_true(all(features$dict$window_hours == 24L))
  expect_true(isTRUE(features$audit))

  expect_equal(nrow(features$x), nrow(cohort))
  expect_equal(length(features$y), nrow(features$x))
  expect_true(all(features$x$icustay_id %in% cohort$icustay_id))
})

test_that("audit_no_leakage hard-fails on a synthetic post-window feature", {
  bad_dict <- data.table::data.table(
    variable        = c("hr_min", "los_hours"),
    source          = c("CHARTEVENTS", "ICUSTAYS"),
    transformation  = c("min over [0,24)h", "discharge - admit"),
    clinical_group  = c("vitals", "FORBIDDEN"),
    window_hours    = c(24L, 24L)
  )
  expect_error(audit_no_leakage(bad_dict, window_hours = 24L),
               regexp = "los", ignore.case = TRUE)
})

test_that("audit_no_leakage rejects mixed window_hours", {
  bad_dict <- data.table::data.table(
    variable        = c("hr_min", "spo2_min"),
    source          = c("CHARTEVENTS", "CHARTEVENTS"),
    transformation  = c("min over [0,24)h", "min over [0,12)h"),
    clinical_group  = c("vitals", "vitals"),
    window_hours    = c(24L, 12L)
  )
  expect_error(audit_no_leakage(bad_dict, window_hours = 24L),
               regexp = "mixed window_hours")
})
