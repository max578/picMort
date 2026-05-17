test_that("make_train_test_split returns stratified halves of expected size", {
  features <- list(y = c(rep(0L, 90), rep(1L, 10)))
  s <- make_train_test_split(features, prop = 0.7, seed = 1L)
  expect_equal(length(s$train_idx) + length(s$test_idx), 100L)
  expect_equal(length(intersect(s$train_idx, s$test_idx)), 0L)
  expect_true(abs(mean(features$y[s$train_idx]) - 0.10) < 0.05)
})

test_that("fit_elastic_net + predict_mortality round-trip on the cohort (gate G4)", {
  pic_root <- file.path(find_project_root(), "data_links", "pic_v110")
  skip_if_not(dir.exists(pic_root), "PIC source not linked at data_links/pic_v110/")

  paths    <- pic_paths()
  cohort   <- build_cohort(paths, min_los_hours = 24L, verbose = FALSE)
  features <- build_features(cohort, paths, window_hours = 24L)
  split    <- make_train_test_split(features)
  fit      <- fit_elastic_net(features, split$train_idx)

  expect_equal(fit$type, "glmnet")
  expect_true(fit$best_alpha >= 0 && fit$best_alpha <= 1)
  expect_true(fit$best_lambda > 0)

  preds <- predict_mortality(fit, features$x[split$test_idx, ])
  expect_equal(nrow(preds), length(split$test_idx))
  expect_true(all(preds$prob_raw >= 0 & preds$prob_raw <= 1))
})

test_that("fit_xgboost + predict_mortality round-trip on the cohort (gate G4)", {
  pic_root <- file.path(find_project_root(), "data_links", "pic_v110")
  skip_if_not(dir.exists(pic_root), "PIC source not linked at data_links/pic_v110/")

  paths    <- pic_paths()
  cohort   <- build_cohort(paths, min_los_hours = 24L, verbose = FALSE)
  features <- build_features(cohort, paths, window_hours = 24L)
  split    <- make_train_test_split(features)
  fit      <- fit_xgboost(features, split$train_idx)

  expect_equal(fit$type, "xgboost")
  expect_true(fit$best_nrounds > 0L)
  expect_true(fit$scale_pos_weight > 1)

  preds <- predict_mortality(fit, features$x[split$test_idx, ])
  expect_equal(nrow(preds), length(split$test_idx))
  expect_true(all(preds$prob_raw >= 0 & preds$prob_raw <= 1))
})

test_that("predict_mortality dispatches PIM3 tables correctly", {
  pic_root <- file.path(find_project_root(), "data_links", "pic_v110")
  skip_if_not(dir.exists(pic_root), "PIC source not linked at data_links/pic_v110/")

  paths    <- pic_paths()
  cohort   <- build_cohort(paths, min_los_hours = 24L, verbose = FALSE)
  pim3_tbl <- compute_pim3(cohort, paths)
  preds <- predict_mortality(pim3_tbl, cohort[1:10, ])
  expect_equal(nrow(preds), 10L)
  expect_true(all(preds$prob_raw >= 0 & preds$prob_raw <= 1))
})

test_that("fit_bayes_horseshoe smoke test (only when PICMORT_TEST_BAYES=true)", {
  skip_if(Sys.getenv("PICMORT_TEST_BAYES") != "true",
          "set PICMORT_TEST_BAYES=true to run brms smoke test (slow)")
  pic_root <- file.path(find_project_root(), "data_links", "pic_v110")
  skip_if_not(dir.exists(pic_root), "PIC source not linked at data_links/pic_v110/")
  skip_if_not_installed("brms")

  paths    <- pic_paths()
  cohort   <- build_cohort(paths, min_los_hours = 24L, verbose = FALSE)
  features <- build_features(cohort, paths, window_hours = 24L)
  split    <- make_train_test_split(features)
  fit      <- fit_bayes_horseshoe(features, split$train_idx,
                                   chains = 1L, iter = 200L)
  expect_equal(fit$type, "bayes_horseshoe")
  preds <- predict_mortality(fit, features$x[split$test_idx, ])
  expect_named(preds, c("prob_raw","prob_calibrated","prob_lower","prob_upper"))
  expect_true(all(preds$prob_lower <= preds$prob_raw))
  expect_true(all(preds$prob_upper >= preds$prob_raw))
})
