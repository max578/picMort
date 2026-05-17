test_that("calibration_suite returns slope/intercept/ICI/CIL with CIs", {
  set.seed(1L)
  n <- 1000
  prob <- runif(n, 0.02, 0.6)
  y <- rbinom(n, 1, prob)
  cs <- calibration_suite(prob, y, n_boot = 100L)
  expect_named(cs, c("slope","intercept","ici","cit_large","curve","n","n_events"))
  expect_named(cs$slope, c("estimate","lower","upper"))
  expect_true(abs(cs$slope[1] - 1) < 0.5)        # well-calibrated by construction
  expect_true(abs(cs$intercept[1]) < 0.5)
})

test_that("discrimination_metrics returns 4 metrics per model with CIs", {
  set.seed(1L)
  n <- 500
  y <- rbinom(n, 1, 0.1)
  probs <- list(
    good = pmin(pmax(y * 0.7 + runif(n, 0, 0.3), 0), 1),
    bad  = runif(n)
  )
  dm <- discrimination_metrics(probs, y, reference = "bad", n_boot = 100L)
  expect_setequal(unique(dm$model), c("good","bad"))
  expect_setequal(unique(dm$metric), c("auroc","auprc","brier","brier_skill"))
  good_auroc <- dm[model == "good" & metric == "auroc"]$estimate
  bad_auroc  <- dm[model == "bad"  & metric == "auroc"]$estimate
  expect_gt(good_auroc, bad_auroc)
})

test_that("AUROC calculation is tie-aware", {
  expect_equal(picMort:::simple_auroc(c(0.5, 0.5), c(0L, 1L)), 0.5)
})

test_that("decision_curve returns model + treat_all + treat_none rows", {
  set.seed(1L)
  n <- 200
  probs <- list(m = runif(n))
  y <- rbinom(n, 1, 0.1)
  dc <- decision_curve(probs, y, thresholds = c(0.05, 0.10, 0.20),
                       plot_grid = FALSE)
  expect_setequal(unique(dc$type), c("model","all","none"))
  expect_equal(nrow(dc[type == "model"]), 3L)
  expect_equal(nrow(dc[type == "all"]),   3L)
  expect_equal(nrow(dc[type == "none"]),  3L)
  expect_true(all(dc[type == "none"]$net_benefit == 0))
})

test_that("subgroup_performance honours the small-cell suppression", {
  cohort_test <- data.table::data.table(
    age_years = c(0.5, 0.5, 7, 7, 14, 14, 14, 14),
    is_surgical = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    primary_icd_chapter = factor(c("respiratory","perinatal","respiratory",
                                   "perinatal","respiratory","perinatal",
                                   "respiratory","perinatal")),
    hospital_expire_flag = c(0L, 1L, 0L, 0L, 0L, 1L, 0L, 0L)
  )
  probs <- list(m = c(0.05, 0.5, 0.04, 0.03, 0.06, 0.45, 0.05, 0.07))
  sg <- subgroup_performance(probs, cohort_test, n_min_events = 5L)
  ## With only 2 events, every cell suppressed -> 0 rows
  expect_equal(nrow(sg), 0L)
  expect_no_warning(
    sg2 <- subgroup_performance(probs, cohort_test, n_min_events = 1L)
  )
  expect_gt(nrow(sg2), 0L)
})

test_that("plot_calibration + plot_decision_curve return ggplot objects", {
  skip_if_not_installed("ggplot2")
  set.seed(1L)
  prob <- runif(200, 0, 1); y <- rbinom(200, 1, prob)
  cs <- list(m = calibration_suite(prob, y, n_boot = 50L))
  dc <- decision_curve(list(m = prob), y)
  expect_s3_class(plot_calibration(cs), "ggplot")
  expect_s3_class(plot_decision_curve(dc), "ggplot")
})
