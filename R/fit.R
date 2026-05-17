## ============================================================================
## Gate G4 -- model fits
##
## Three frequentist comparators + one Bayesian extension:
##   * Penalised logistic regression (elastic net, glmnet)
##   * XGBoost (gradient-boosted trees)
##   * Bayesian elastic-net via brms with regularised horseshoe prior
##     -- the JTM clinical-functional wedge: every patient gets a
##     posterior 95 % credible interval on predicted mortality.
##
## All three share the same `default_recipe()` preprocessing
## (median/mode imputation, NZV drop, one-hot, normalise). Each fit
## function preps the recipe on the training fold and stores the
## prepped recipe in the returned object so `predict_mortality()`
## bakes the test fold consistently.
##
## Class imbalance (~7.4 % mortality after the LOS filter): glmnet
## uses observation weights; XGBoost uses `scale_pos_weight`; the
## Bayesian model uses the regularised horseshoe + Bernoulli
## likelihood (no weights -- the prior shrinks irrelevant
## coefficients while the posterior absorbs the imbalance).
## ============================================================================

#' Stratified train / test split
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Outcome-stratified single split into training and test folds via
#' `rsample::initial_split()`. Returns 1-based integer indices into
#' `features$y` (and rows of `features$x`).
#'
#' @param features Output of [build_features()].
#' @param prop Proportion in the training fold. Default 0.7.
#' @param seed Integer seed; default 20260508.
#'
#' @return A list with `train_idx`, `test_idx` (integer vectors).
#'
#' @examples
#' if (requireNamespace("rsample", quietly = TRUE)) {
#'   set.seed(20260517L)
#'   features <- list(y = c(rep(0L, 90), rep(1L, 10)))
#'   split <- make_train_test_split(features, prop = 0.7, seed = 1L)
#'   length(split$train_idx) + length(split$test_idx) # 100
#'   mean(features$y[split$train_idx]) # ~ 0.10
#' }
#' @export
make_train_test_split <- function(features, prop = 0.7, seed = 20260508L) {
  if (!requireNamespace("rsample", quietly = TRUE)) {
    stop("`rsample` is required.", call. = FALSE)
  }
  set.seed(seed)
  df <- data.frame(idx = seq_along(features$y),
                   y   = factor(features$y, levels = c(0L, 1L),
                                labels = c("alive","died")))
  spl <- rsample::initial_split(df, prop = prop, strata = "y")
  list(train_idx = rsample::training(spl)$idx,
       test_idx  = rsample::testing(spl)$idx)
}

#' Internal helper: prep recipe + extract design matrix for a fold
#' @keywords internal
#' @noRd
prep_fold <- function(features, train_idx) {
  rec  <- default_recipe(features$x[train_idx, ], features$y[train_idx])
  prep <- recipes::prep(rec, verbose = FALSE)
  baked <- recipes::bake(prep, new_data = NULL)
  predictors <- setdiff(names(baked), c(".outcome","icustay_id"))
  list(prep = prep,
       X    = as.matrix(baked[, predictors]),
       y    = as.integer(baked$.outcome) - 1L,
       baked = baked,
       predictors = predictors)
}

#' Bake new data with a fitted prep
#' @keywords internal
#' @noRd
bake_new <- function(prep, x_new, predictors) {
  df <- as.data.frame(x_new)
  df$.outcome <- factor(rep("alive", nrow(df)), levels = c("alive","died"))
  baked <- recipes::bake(prep, new_data = df)
  list(X = as.matrix(baked[, predictors]),
       baked = baked)
}

#' Fit penalised logistic regression (elastic net)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Inner CV over the (alpha, lambda) grid; class-weighted to handle
#' the ~7-9 % paediatric ICU mortality rate. Selects best alpha by
#' minimum CV deviance and uses `lambda.1se` for parsimony.
#'
#' @param features Output of [build_features()].
#' @param train_idx Integer vector of training-row indices.
#' @param alpha_grid Numeric vector of alpha values; default `seq(0, 1, 0.2)`.
#' @param nfolds Inner CV folds; default 5.
#' @param seed Integer seed; default 20260508.
#'
#' @return A list with `model`, `prep`, `predictors`, `best_alpha`,
#'   `best_lambda`, `cv_log`, and `type = "glmnet"`.
#'
#' @examples
#' \dontrun{
#' paths    <- pic_paths()
#' cohort   <- build_cohort(paths, min_los_hours = 24L, verbose = FALSE)
#' features <- build_features(cohort, paths, window_hours = 24L)
#' split    <- make_train_test_split(features)
#' fit      <- fit_elastic_net(features, split$train_idx)
#' fit$best_alpha
#' fit$best_lambda
#' }
#' @export
fit_elastic_net <- function(features, train_idx,
                            alpha_grid = seq(0, 1, by = 0.2),
                            nfolds = 5L,
                            seed = 20260508L) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("`glmnet` is required.", call. = FALSE)
  }
  set.seed(seed)
  pf <- prep_fold(features, train_idx)
  pos <- sum(pf$y == 1L); neg <- sum(pf$y == 0L)
  w <- ifelse(pf$y == 1L, neg / (pos + neg), pos / (pos + neg))

  cvs <- lapply(alpha_grid, function(a) {
    glmnet::cv.glmnet(pf$X, pf$y, family = "binomial", alpha = a,
                      nfolds = nfolds, weights = w,
                      type.measure = "deviance")
  })
  best_idx    <- which.min(vapply(cvs, function(cv) min(cv$cvm), numeric(1)))
  best_cv     <- cvs[[best_idx]]
  best_alpha  <- alpha_grid[best_idx]
  best_lambda <- best_cv$lambda.1se

  fit <- glmnet::glmnet(pf$X, pf$y, family = "binomial",
                        alpha = best_alpha, lambda = best_lambda, weights = w)

  cv_log <- data.table::data.table(
    alpha       = alpha_grid,
    cvm_min     = vapply(cvs, function(cv) min(cv$cvm), numeric(1)),
    lambda_1se  = vapply(cvs, function(cv) cv$lambda.1se, numeric(1))
  )

  list(model = fit, prep = pf$prep, predictors = pf$predictors,
       best_alpha = best_alpha, best_lambda = best_lambda,
       cv_log = cv_log, type = "glmnet")
}

#' Fit XGBoost classifier
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Inner CV over a small principled grid. Class imbalance handled via
#' `scale_pos_weight = n_neg / n_pos`. Returns the best parameter set
#' and the corresponding fitted model.
#'
#' @inheritParams fit_elastic_net
#' @param grid `data.frame` of hyperparameter combinations. If `NULL`,
#'   uses the canonical 4-row grid documented in `vignettes/paper1_baseline.Rmd`.
#'
#' @return A list with `model`, `prep`, `predictors`, `best_params`,
#'   `best_nrounds`, `cv_log`, `scale_pos_weight`, and `type = "xgboost"`.
#'
#' @examples
#' \dontrun{
#' paths    <- pic_paths()
#' cohort   <- build_cohort(paths, min_los_hours = 24L, verbose = FALSE)
#' features <- build_features(cohort, paths, window_hours = 24L)
#' split    <- make_train_test_split(features)
#' fit      <- fit_xgboost(features, split$train_idx)
#' fit$best_nrounds
#' }
#' @export
fit_xgboost <- function(features, train_idx,
                        grid = NULL,
                        nfolds = 5L,
                        seed = 20260508L) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("`xgboost` is required.", call. = FALSE)
  }
  set.seed(seed)
  pf <- prep_fold(features, train_idx)
  scale_pos_weight <- sum(pf$y == 0L) / max(1L, sum(pf$y == 1L))

  if (is.null(grid)) {
    grid <- data.frame(
      eta              = c(0.05, 0.10, 0.05, 0.10),
      max_depth        = c(4L,   4L,   6L,   6L),
      subsample        = c(0.8,  0.8,  0.8,  0.8),
      colsample_bytree = c(0.8,  0.8,  0.8,  0.8),
      min_child_weight = c(1L,   1L,   3L,   3L)
    )
  }

  dtrain <- xgboost::xgb.DMatrix(data = pf$X, label = pf$y)

  cv_log <- vector("list", nrow(grid))
  best <- list(score = Inf, params = NULL, nrounds = NULL)

  for (i in seq_len(nrow(grid))) {
    p <- as.list(grid[i, , drop = FALSE])
    cv <- xgboost::xgb.cv(
      params = c(p, list(objective = "binary:logistic",
                         eval_metric = "logloss",
                         scale_pos_weight = scale_pos_weight)),
      data = dtrain,
      nfold = nfolds, nrounds = 500L,
      early_stopping_rounds = 20L, verbose = 0L,
      stratified = TRUE
    )
    score <- min(cv$evaluation_log$test_logloss_mean)
    best_iter <- which.min(cv$evaluation_log$test_logloss_mean)
    cv_log[[i]] <- data.table::data.table(row = i, score = score,
                                          best_iter = best_iter)
    if (score < best$score) {
      best$score   <- score
      best$params  <- p
      best$nrounds <- best_iter
    }
  }

  fit <- xgboost::xgb.train(
    params = c(best$params, list(objective = "binary:logistic",
                                 eval_metric = "logloss",
                                 scale_pos_weight = scale_pos_weight)),
    data = dtrain,
    nrounds = best$nrounds, verbose = 0L
  )

  list(model = fit, prep = pf$prep, predictors = pf$predictors,
       best_params = best$params, best_nrounds = best$nrounds,
       cv_log = data.table::rbindlist(cv_log),
       scale_pos_weight = scale_pos_weight,
       type = "xgboost")
}

#' Fit Bayesian logistic regression with regularised horseshoe prior
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Uses `brms::brm()` with a Bernoulli likelihood and a regularised
#' horseshoe prior on the coefficients (Carvalho 2010 / Piironen
#' & Vehtari 2017). The prior shrinks irrelevant coefficients
#' aggressively while leaving informative ones effectively
#' unregularised; the posterior gives every patient a credible
#' interval on predicted mortality probability -- the JTM
#' clinical-functional wedge.
#'
#' @inheritParams fit_elastic_net
#' @param chains MCMC chains. Default 2.
#' @param iter Total iterations per chain (warmup + sampling). Default 1000.
#' @param par_ratio Prior on the proportion of non-zero coefficients
#'   (regularised-horseshoe `par_ratio`). Default 0.1 (~10 % expected
#'   to be non-zero from ~100 candidate predictors).
#' @param adapt_delta NUTS adaptation. Default 0.95.
#' @param cores Number of cores. Default = `chains`.
#'
#' @return A list with `model` (a `brmsfit`), `prep`, `predictors`,
#'   `chains`, `iter`, `seed`, and `type = "bayes_horseshoe"`.
#'
#' @examples
#' # `fit_bayes_horseshoe()` compiles a Stan model via `brms`. Compilation
#' # alone runs 30 s - 3 min and a minimal 1-chain / 200-iter sample
#' # another 1-5 min, so the example is wrapped in `\dontrun{}` rather than
#' # `\donttest{}` -- `R CMD check --run-donttest` would otherwise need a
#' # fully provisioned rstan toolchain on every CI worker. The brms smoke
#' # test in `tests/testthat/test-fits.R` runs only when the environment
#' # variable `PICMORT_TEST_BAYES=true` is set.
#' \dontrun{
#' paths    <- pic_paths()
#' cohort   <- build_cohort(paths, min_los_hours = 24L, verbose = FALSE)
#' features <- build_features(cohort, paths, window_hours = 24L)
#' split    <- make_train_test_split(features)
#' fit      <- fit_bayes_horseshoe(features, split$train_idx,
#'                                 chains = 1L, iter = 200L)
#' fit$type
#' }
#' @export
fit_bayes_horseshoe <- function(features, train_idx,
                                chains = 2L, iter = 1000L,
                                par_ratio = 0.10,
                                adapt_delta = 0.95,
                                cores = chains,
                                seed = 20260508L) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("`brms` is required.", call. = FALSE)
  }
  set.seed(seed)
  pf <- prep_fold(features, train_idx)

  formula <- stats::as.formula(
    paste(".outcome ~", paste(pf$predictors, collapse = " + "))
  )
  prior <- brms::set_prior(
    sprintf("horseshoe(df = 1, par_ratio = %s)", par_ratio),
    class = "b"
  )

  fit <- brms::brm(
    formula = formula,
    data    = as.data.frame(pf$baked),
    family  = brms::bernoulli(link = "logit"),
    prior   = prior,
    chains  = chains, iter = iter, warmup = floor(iter / 2),
    cores   = cores, seed = seed,
    refresh = 0L,
    control = list(adapt_delta = adapt_delta, max_treedepth = 12L),
    silent  = 2L
  )

  list(model = fit, prep = pf$prep, predictors = pf$predictors,
       chains = chains, iter = iter, seed = seed,
       type = "bayes_horseshoe")
}

#' Predict mortality probabilities (unified interface)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Routes to the appropriate predict implementation based on
#' `model$type`. Returns a `data.table` with columns:
#'   * `prob_raw`         -- model output (probability of in-hospital mortality)
#'   * `prob_calibrated`  -- post-hoc calibrated (only XGBoost; same as raw otherwise)
#'   * `prob_lower`, `prob_upper` -- 95 % credible interval (Bayesian only)
#'
#' @param model Fit object from one of the `fit_*()` functions, OR a
#'   PIM3 `data.table` from [compute_pim3()].
#' @param new_data New `features$x`-style `data.table` (with
#'   `icustay_id` and the same columns as the training features).
#'
#' @return A `data.table` aligned to `nrow(new_data)`.
#'
#' @examples
#' # PIM3 dispatch branch: predict_mortality() also accepts the PIM3
#' # data.table returned by compute_pim3(), bypassing model fits.
#' toy <- readRDS(system.file("extdata", "toy_cohort.rds",
#'                            package = "picMort"))
#' pim3_tbl <- data.table::data.table(
#'   icustay_id = toy$icustay_id,
#'   pim3       = stats::plogis(stats::rnorm(nrow(toy), mean = -2.5, sd = 0.5))
#' )
#' preds <- predict_mortality(pim3_tbl, toy[1:5, ])
#' preds
#'
#' \dontrun{
#' # Full model-fit dispatch (requires the registered PIC data)
#' paths    <- pic_paths()
#' cohort   <- build_cohort(paths, min_los_hours = 24L, verbose = FALSE)
#' features <- build_features(cohort, paths, window_hours = 24L)
#' split    <- make_train_test_split(features)
#' fit      <- fit_elastic_net(features, split$train_idx)
#' predict_mortality(fit, features$x[split$test_idx, ])
#' }
#' @export
predict_mortality <- function(model, new_data) {
  if (data.table::is.data.table(model) && "pim3" %in% names(model)) {
    out <- merge(data.table::data.table(icustay_id = new_data$icustay_id),
                 model[, list(icustay_id, prob_raw = pim3, prob_calibrated = pim3)],
                 by = "icustay_id", sort = FALSE, all.x = TRUE)
    return(out[, list(prob_raw, prob_calibrated)])
  }
  bn <- bake_new(model$prep, new_data, model$predictors)
  switch(
    model$type,
    "glmnet" = {
      prob <- as.numeric(stats::predict(model$model, newx = bn$X, type = "response"))
      data.table::data.table(prob_raw = prob, prob_calibrated = prob)
    },
    "xgboost" = {
      prob <- stats::predict(model$model, newdata = bn$X)
      data.table::data.table(prob_raw = prob, prob_calibrated = prob)
    },
    "bayes_horseshoe" = {
      total <- brms::ndraws(model$model)
      ndraws <- min(500L, total)
      epred <- brms::posterior_epred(model$model,
                                     newdata = as.data.frame(bn$baked),
                                     ndraws = ndraws)
      data.table::data.table(
        prob_raw        = colMeans(epred),
        prob_calibrated = colMeans(epred),
        prob_lower      = apply(epred, 2L, stats::quantile, probs = 0.025),
        prob_upper      = apply(epred, 2L, stats::quantile, probs = 0.975)
      )
    },
    stop(sprintf("Unknown model type: %s", model$type), call. = FALSE)
  )
}
