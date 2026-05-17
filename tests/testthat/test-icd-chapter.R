test_that("icd10_to_chapter dispatches the right chapter for sample codes", {
  expect_equal(icd10_to_chapter("J18.900"), "respiratory")
  expect_equal(icd10_to_chapter("K52.901"), "digestive")
  expect_equal(icd10_to_chapter("Q43.901"), "congenital")
  expect_equal(icd10_to_chapter("P07.000"), "perinatal")
  expect_equal(icd10_to_chapter("S06.500"), "injury")
  expect_equal(icd10_to_chapter("I50.900"), "circulatory")
  expect_equal(icd10_to_chapter("D70.000"), "blood")
  expect_equal(icd10_to_chapter("C71.900"), "neoplasms")
  expect_equal(icd10_to_chapter("D49.900"), "neoplasms")
  expect_equal(icd10_to_chapter("D50.900"), "blood")
  expect_equal(icd10_to_chapter("H10.000"), "eye")
  expect_equal(icd10_to_chapter("H66.900"), "ear")
})

test_that("icd10_to_chapter handles NA and empty input", {
  expect_true(is.na(icd10_to_chapter(NA_character_)))
  expect_true(is.na(icd10_to_chapter("")))
  expect_equal(icd10_to_chapter(c("J18.9", NA, "K52.9")),
               c("respiratory", NA, "digestive"))
})
