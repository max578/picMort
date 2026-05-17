test_that("cohort invariants hold on the canonical extraction (gate G1)", {
  pic_root <- file.path(find_project_root(), "data_links", "pic_v110")
  skip_if_not(dir.exists(pic_root), "PIC source not linked at data_links/pic_v110/")

  paths  <- pic_paths()
  cohort <- build_cohort(paths, min_los_hours = 24L, verbose = FALSE)

  expect_s3_class(cohort, "data.table")
  expect_setequal(
    names(cohort),
    c("subject_id","hadm_id","icustay_id","intime","outtime","los_hours",
      "age_months","age_years","sex",
      "hospital_expire_flag","admit_year","is_surgical","primary_icd_chapter")
  )
  expect_true(data.table::uniqueN(cohort$subject_id) == nrow(cohort),
              info = "first-stay-per-patient invariant")
  expect_true(all(cohort$age_years >= 0 & cohort$age_years <= 18))
  expect_true(all(cohort$los_hours >= 24))

  expect_silent(assert_cohort_invariants(cohort))

  attr_tbl <- attr(cohort, "attrition")
  expect_s3_class(attr_tbl, "data.table")
  expect_true("los_ge_24h" %in% attr_tbl$step)
})

test_that("default cohort expectations are well-formed", {
  expected <- picMort:::default_cohort_expectations()
  expect_named(expected,
    c("n_min","n_max","mortality_rate","age_range_years",
      "sex_levels","distinct_subject","no_overlap_stays"),
    ignore.order = TRUE
  )
  expect_lt(expected$n_min, expected$n_max)
  expect_lt(expected$mortality_rate[1], expected$mortality_rate[2])
})

test_that("assert_cohort_invariants raises on a deliberately broken cohort", {
  bad <- data.table::data.table(
    subject_id = 1:10,
    hadm_id = 1:10,
    icustay_id = 1:10,
    age_years = c(seq(0, 17, length.out = 9), 25),  # one out-of-range
    sex = factor(rep(c("F","M"), 5), levels = c("F","M")),
    hospital_expire_flag = c(rep(0L, 9), 1L),
    los_hours = rep(48, 10)
  )
  expect_error(assert_cohort_invariants(bad), regexp = "age range")
})
