test_that("pic_paths() returns the canonical file map", {
  skip_if_not(dir.exists(file.path(find_project_root(), "data_links", "pic_v110")),
              "PIC source not linked at data_links/pic_v110/")
  paths <- pic_paths(check = FALSE)
  expect_named(paths,
    c("admissions","patients","icustays","chartevents","labevents",
      "diagnoses_icd","d_items","d_icd_diagnoses","d_labitems",
      "prescriptions","inputevents","outputevents","microbiologyevents",
      "surgery_vital_signs","or_exam_reports","emr_symptoms"),
    ignore.order = TRUE
  )
})

test_that("verify_pic_paths() raises on missing files", {
  fake <- list(admissions = fs::path(tempdir(), "NO_SUCH_FILE.csv"))
  expect_error(picMort:::verify_pic_paths(fake), regexp = "not reachable")
})
