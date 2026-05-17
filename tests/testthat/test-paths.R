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

test_that("pic_paths() honours PICMORT_DATA_DIR", {
  old <- Sys.getenv("PICMORT_DATA_DIR", unset = NA_character_)
  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("PICMORT_DATA_DIR")
    } else {
      Sys.setenv(PICMORT_DATA_DIR = old)
    }
  }, add = TRUE)

  tmp <- tempfile()
  dir.create(tmp)
  Sys.setenv(PICMORT_DATA_DIR = tmp)
  paths <- pic_paths(check = FALSE)
  expect_equal(paths$admissions, fs::path(tmp, "ADMISSIONS.csv"))
  unlink(tmp, recursive = TRUE)
})

test_that("verify_pic_paths() raises on missing files", {
  fake <- list(admissions = fs::path(tempdir(), "NO_SUCH_FILE.csv"))
  expect_error(picMort:::verify_pic_paths(fake), regexp = "not reachable")
})
