## ============================================================================
## Gate G5/G6 -- evaluation suite
##
## Headline framing for the manuscript: calibration + decision-curve
## analysis. Discrimination (AUROC, AUPRC, Brier) is supporting.
## Subgroup performance reports per age strata, surgical/medical and
## top ICD chapters with small-cell suppression at n_min < 50.
##
## Calibration and discrimination metrics support bootstrap percentile CIs
## (default 1,000 reps). Bayesian model predictions can additionally carry
## posterior
## CrIs natively in `prob_lower` / `prob_upper`; calibration_suite
## reports both bootstrap and (optional) Bayesian uncertainty when
## available.
## ============================================================================

#' Calibration suite (the manuscript headline)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Computes calibration slope, intercept, integrated calibration index
#' (ICI), calibration-in-the-large, and a smoothed calibration curve
#' (`loess`). Bootstrap percentile CIs (1,000 reps default) on every
#' point estimate.
#'
#' Definitions:
#'   * **Calibration slope**: logistic regression of `y ~ logit(prob)`,
#'     slope coefficient. Ideal = 1; <1 indicates over-fitting.
#'   * **Calibration intercept**: logistic regression of
#'     `y ~ offset(logit(prob))`, intercept. Ideal = 0; >0 indicates
#'     systematic under-prediction.
#'   * **Calibration-in-the-large**: `log(prev_obs / prev_pred)`.
#'     Ideal = 0.
#'   * **ICI**: mean absolute difference between smoothed observed and
#'     predicted probabilities.
#'
#' @param prob Numeric vector of predicted probabilities.
#' @param y Outcome (0/1 integer).
#' @param n_boot Bootstrap replicates; default 1000.
#' @param seed Integer seed; default 20260508.
#'
#' @return A list with elements `slope`, `intercept`, `ici`, `cit_large`
#'   (each named `c(estimate, lower, upper)`), `curve` (a `data.frame`
#'   for plotting), and `n`, `n_events`.
#'
#' @examples
#' set.seed(20260517L)
#' n <- 200L
#' prob <- stats::plogis(stats::rnorm(n, mean = -2, sd = 1))
#' y    <- stats::rbinom(n, 1L, prob)
#' cal  <- calibration_suite(prob, y, n_boot = 50L)
#' cal$slope
#' cal$ici
#' head(cal$curve)
#' @export
calibration_suite <- function(prob, y, n_boot = 1000L, seed = 20260508L) {
  prob <- pmax(pmin(as.numeric(prob), 1 - 1e-7), 1e-7)
  y <- as.integer(y)
  stopifnot(length(prob) == length(y))
  n <- length(y); n_events <- sum(y)

  point <- calibration_metrics_point(prob, y)
  curve <- calibration_curve_points(prob, y)

  set.seed(seed)
  boot <- replicate(n_boot, {
    idx <- sample.int(n, n, replace = TRUE)
    calibration_metrics_point(prob[idx], y[idx])
  })

  ci <- function(name) {
    v <- boot[name, ]
    v <- v[is.finite(v)]
    if (length(v) == 0L) {
      return(c(estimate = unname(point[name]),
               lower = NA_real_,
               upper = NA_real_))
    }
    c(estimate = unname(point[name]),
      lower = stats::quantile(v, 0.025, names = FALSE),
      upper = stats::quantile(v, 0.975, names = FALSE))
  }

  list(
    slope     = ci("slope"),
    intercept = ci("intercept"),
    ici       = ci("ici"),
    cit_large = ci("cit_large"),
    curve     = curve,
    n         = n,
    n_events  = n_events
  )
}

#' @keywords internal
#' @noRd
calibration_metrics_point <- function(prob, y) {
  logit_p <- stats::qlogis(prob)
  slope <- safe_glm_coef(
    stats::glm(y ~ logit_p, family = stats::binomial()),
    index = 2L
  )
  intercept <- safe_glm_coef(
    stats::glm(y ~ offset(logit_p), family = stats::binomial()),
    index = 1L
  )
  loess_fit <- safe_loess(prob, y)
  ici <- if (is.null(loess_fit)) NA_real_ else
    mean(abs(stats::fitted(loess_fit) - prob))
  cit_large <- log((mean(y) + 1e-9) / (mean(prob) + 1e-9))
  c(
    slope     = slope,
    intercept = intercept,
    ici       = ici,
    cit_large = cit_large
  )
}

#' @keywords internal
#' @noRd
safe_glm_coef <- function(expr, index) {
  fit <- tryCatch(
    suppressWarnings(expr),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NA_real_)
  co <- stats::coef(fit)
  if (length(co) < index || !is.finite(co[[index]])) return(NA_real_)
  unname(co[[index]])
}

#' @keywords internal
#' @noRd
safe_loess <- function(prob, y) {
  if (length(prob) < 4L ||
      length(unique(prob)) < 3L ||
      length(unique(y)) < 2L) {
    return(NULL)
  }
  tryCatch(
    suppressWarnings(stats::loess(y ~ prob, span = 0.75, degree = 1)),
    error = function(e) NULL
  )
}

#' @keywords internal
#' @noRd
calibration_curve_points <- function(prob, y, ngrid = 200L) {
  loess_fit <- safe_loess(prob, y)
  if (is.null(loess_fit)) {
    return(data.frame(prob = numeric(), observed = numeric()))
  }
  grid <- seq(min(prob), max(prob), length.out = ngrid)
  observed <- stats::predict(loess_fit, newdata = data.frame(prob = grid))
  observed <- pmax(pmin(observed, 1), 0)
  data.frame(prob = grid, observed = observed)
}

#' Decision-curve analysis at clinically meaningful thresholds
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Computes net benefit per threshold per model alongside the
#' "treat-all" / "treat-none" references. Net benefit is computed
#' analytically (no external package dependency); equivalent to
#' `dcurves::dca()`.
#'
#' @param probs Named list of predicted-probability vectors.
#' @param y Outcome (0/1 integer).
#' @param thresholds Numeric vector of threshold probabilities; default
#'   `c(0.05, 0.10, 0.20)` plus a fine grid for plotting.
#' @param plot_grid If `TRUE` (default), additionally returns a fine
#'   grid of thresholds (every 1 %) for plot rendering.
#' @param n_boot Bootstrap replicates for paired CIs on net benefit
#'   at the prespecified `thresholds` and on the Brier skill score.
#'   Default 0 (no CIs); typical production value 1000.
#' @param seed Integer seed for the bootstrap (used only when
#'   `n_boot > 0`). Default 20260508.
#'
#' @return A `data.table` of (model, threshold, net_benefit, type)
#'   when `n_boot = 0`, where `type` is `"model"`, `"all"`, or
#'   `"none"`. When `n_boot > 0`, additional rows with
#'   `(model, metric, estimate, lower, upper)` columns carry paired
#'   bootstrap 95 % CIs on net benefit at each prespecified threshold
#'   and on the Brier skill score.
#'
#' @examples
#' set.seed(20260517L)
#' n <- 200L
#' p_a <- stats::plogis(stats::rnorm(n, -2, 1))
#' p_b <- stats::plogis(stats::rnorm(n, -2, 1.4))
#' y   <- stats::rbinom(n, 1L, (p_a + p_b) / 2)
#' dca <- decision_curve(list(model_a = p_a, model_b = p_b), y,
#'                       thresholds = c(0.05, 0.10, 0.20),
#'                       plot_grid = FALSE)
#' dca
#' @export
decision_curve <- function(probs, y,
                            thresholds = c(0.05, 0.10, 0.20),
                            plot_grid = TRUE,
                            n_boot = 0L,
                            seed = 20260508L) {
  y <- as.integer(y)
  prev <- mean(y)
  n <- length(y)
  threshold_grid <- if (isTRUE(plot_grid))
    sort(unique(c(thresholds, seq(0.005, 0.50, by = 0.005))))
  else thresholds

  nb_at <- function(p, y, pt) {
    nn <- length(y)
    pred_pos <- p >= pt
    tp <- sum(pred_pos & y == 1L)
    fp <- sum(pred_pos & y == 0L)
    (tp / nn) - (fp / nn) * (pt / (1 - pt))
  }

  rows <- list()
  for (mod in names(probs)) {
    p <- probs[[mod]]
    for (pt in threshold_grid) {
      rows[[length(rows) + 1L]] <-
        data.table::data.table(model = mod, threshold = pt,
                               net_benefit = nb_at(p, y, pt),
                               type = "model",
                               lower = NA_real_, upper = NA_real_)
    }
  }
  for (pt in threshold_grid) {
    nb_all <- prev - (1 - prev) * (pt / (1 - pt))
    rows[[length(rows) + 1L]] <-
      data.table::data.table(model = "treat_all", threshold = pt,
                             net_benefit = nb_all, type = "all",
                             lower = NA_real_, upper = NA_real_)
    rows[[length(rows) + 1L]] <-
      data.table::data.table(model = "treat_none", threshold = pt,
                             net_benefit = 0, type = "none",
                             lower = NA_real_, upper = NA_real_)
  }
  out <- data.table::rbindlist(rows)

  # Bootstrap CIs at the headline thresholds (cheap: only `thresholds`,
  # not the fine plot grid). Matches the standalone pipeline's
  # `decision_curve(..., n_boot = N_BOOT)` semantics so that the package
  # reproduces the manuscript's per-model CIs identically.
  if (isTRUE(n_boot > 0L) && length(thresholds)) {
    set.seed(seed)
    boot_rows <- list()
    for (mod in names(probs)) {
      p <- probs[[mod]]
      for (pt in thresholds) {
        v <- replicate(n_boot, {
          idx <- sample.int(n, n, replace = TRUE)
          nb_at(p[idx], y[idx], pt)
        })
        v <- v[is.finite(v)]
        boot_rows[[length(boot_rows) + 1L]] <- data.table::data.table(
          model = mod, threshold = pt,
          lower = stats::quantile(v, 0.025, names = FALSE),
          upper = stats::quantile(v, 0.975, names = FALSE)
        )
      }
    }
    boot_dt <- data.table::rbindlist(boot_rows)
    for (i in seq_len(nrow(boot_dt))) {
      out[model == boot_dt$model[i] &
          abs(threshold - boot_dt$threshold[i]) < 1e-9 &
          type == "model",
          `:=`(lower = boot_dt$lower[i], upper = boot_dt$upper[i])]
    }
  }
  out
}

#' Discrimination metrics with bootstrap CIs (supporting role)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' AUROC, AUPRC, Brier score, and Brier skill score relative to a
#' reference model.
#'
#' @param probs Named list of predicted-probability vectors aligned to
#'   `y`.
#' @param y Outcome (0/1 integer).
#' @param reference Name in `names(probs)` to use for the Brier-skill-
#'   score reference. Default `"pim3"` if present, else first model.
#' @param n_boot Bootstrap replicates; default 1000.
#' @param seed Integer seed; default 20260508.
#'
#' @return A `data.table` of (model, metric, estimate, lower, upper).
#'
#' @examples
#' set.seed(20260517L)
#' n <- 200L
#' p_a <- stats::plogis(stats::rnorm(n, -2, 1))
#' p_b <- stats::plogis(stats::rnorm(n, -2, 1.4))
#' y   <- stats::rbinom(n, 1L, (p_a + p_b) / 2)
#' discrimination_metrics(list(model_a = p_a, model_b = p_b), y,
#'                        reference = "model_a", n_boot = 50L)
#' @export
discrimination_metrics <- function(probs, y, reference = NULL,
                                   n_boot = 1000L, seed = 20260508L) {
  y <- as.integer(y); n <- length(y)
  if (is.null(reference)) {
    reference <- if ("pim3" %in% names(probs)) "pim3" else names(probs)[1L]
  }
  ref_brier_full <- mean((probs[[reference]] - y)^2)

  point_metrics_for <- function(mod) {
    p <- probs[[mod]]
    auroc <- simple_auroc(p, y)
    auprc <- simple_auprc(p, y)
    brier <- mean((p - y)^2)
    bss   <- 1 - brier / max(ref_brier_full, .Machine$double.eps)
    c(auroc = auroc, auprc = auprc, brier = brier, brier_skill = bss)
  }

  ## AUROC / AUPRC / Brier: per-model independent bootstrap.
  ## BSS: paired bootstrap with shared indices across all models, using
  ## the bootstrap-sample reference Brier as denominator in each replicate
  ## (the reference model's BSS is then exactly 0 / [0, 0]). Two passes
  ## seeded identically so AUROC/AUPRC/Brier CIs are unchanged from prior
  ## release.
  rows <- list()
  metric_names_indep <- c("auroc", "auprc", "brier")

  set.seed(seed)
  for (mod in names(probs)) {
    p <- probs[[mod]]
    pt <- point_metrics_for(mod)
    boot_mat <- replicate(n_boot, {
      idx <- sample.int(n, n, replace = TRUE)
      c(auroc = simple_auroc(p[idx], y[idx]),
        auprc = simple_auprc(p[idx], y[idx]),
        brier = mean((p[idx] - y[idx])^2))
    })
    for (m in metric_names_indep) {
      v <- boot_mat[m, ]; v <- v[is.finite(v)]
      rows[[length(rows) + 1L]] <- data.table::data.table(
        model = mod, metric = m,
        estimate = unname(pt[m]),
        lower = stats::quantile(v, 0.025, names = FALSE),
        upper = stats::quantile(v, 0.975, names = FALSE)
      )
    }
  }

  set.seed(seed)
  mod_names <- names(probs)
  ref_p     <- probs[[reference]]
  bss_boot  <- matrix(NA_real_, nrow = n_boot, ncol = length(mod_names),
                      dimnames = list(NULL, mod_names))
  for (b in seq_len(n_boot)) {
    idx          <- sample.int(n, n, replace = TRUE)
    y_b          <- y[idx]
    ref_brier_b  <- mean((ref_p[idx] - y_b)^2)
    denom        <- max(ref_brier_b, .Machine$double.eps)
    for (mod in mod_names) {
      brier_b           <- mean((probs[[mod]][idx] - y_b)^2)
      bss_boot[b, mod]  <- 1 - brier_b / denom
    }
  }
  for (mod in mod_names) {
    pt <- point_metrics_for(mod)
    v  <- bss_boot[, mod]; v <- v[is.finite(v)]
    rows[[length(rows) + 1L]] <- data.table::data.table(
      model = mod, metric = "brier_skill",
      estimate = unname(pt["brier_skill"]),
      lower = stats::quantile(v, 0.025, names = FALSE),
      upper = stats::quantile(v, 0.975, names = FALSE)
    )
  }

  data.table::rbindlist(rows)
}

#' @keywords internal
#' @noRd
simple_auroc <- function(p, y) {
  ok <- is.finite(p) & !is.na(y)
  p <- p[ok]
  y <- as.integer(y[ok])
  n_pos <- sum(y == 1L)
  n_neg <- sum(y == 0L)
  if (n_pos == 0L || n_neg == 0L) return(NA_real_)
  ranks <- rank(p, ties.method = "average")
  (sum(ranks[y == 1L]) - n_pos * (n_pos + 1L) / 2) / (n_pos * n_neg)
}

#' @keywords internal
#' @noRd
simple_auprc <- function(p, y) {
  o <- order(p, decreasing = TRUE); y <- y[o]
  if (sum(y) == 0L) return(NA_real_)
  tp <- cumsum(y); fp <- cumsum(1L - y)
  recall <- tp / sum(y)
  precision <- tp / (tp + fp)
  precision[is.nan(precision)] <- 0
  # Trapezoidal area
  dx <- diff(c(0, recall))
  sum(dx * c(precision[1], precision[-length(precision)] + precision[-1]) / 2)
}

#' Subgroup performance table
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Reports calibration + AUROC per pre-registered subgroup: age strata
#' (<1, 1-5, 6-12, 13-18 y), surgical vs medical, and top-3 ICD
#' chapters by frequency. Cells with fewer than `n_min_events` events
#' are suppressed.
#'
#' @param probs Named list of predicted-probability vectors aligned to
#'   `cohort_test`.
#' @param cohort_test Cohort test fold (a `data.table` with
#'   `age_years`, `is_surgical`, `primary_icd_chapter`,
#'   `hospital_expire_flag`).
#' @param n_min_events Minimum events per cell. Default 5.
#'
#' @return A `data.table` of (model, subgroup_var, level, n,
#'   n_events, auroc, ici, cal_slope).
#'
#' @examples
#' toy <- readRDS(system.file("extdata", "toy_cohort.rds",
#'                            package = "picMort"))
#' set.seed(20260517L)
#' probs <- list(model_a = stats::runif(nrow(toy)))
#' # n_min_events = 1L because the toy cohort only has 8 deaths.
#' subgroup_performance(probs, toy, n_min_events = 1L)
#' @export
subgroup_performance <- function(probs, cohort_test, n_min_events = 5L) {
  age_bin <- cut(cohort_test$age_years,
                 breaks = c(-Inf, 1, 5, 12, Inf),
                 labels = c("<1y","1-5y","6-12y","13-18y"),
                 right = FALSE)
  surg <- ifelse(as.logical(cohort_test$is_surgical), "surgical", "medical")
  icd  <- as.character(cohort_test$primary_icd_chapter)
  top_icd <- names(sort(table(icd), decreasing = TRUE))[1:3]
  icd_label <- ifelse(icd %in% top_icd, icd, "other")

  spec <- list(
    list(name = "age",      groups = split(seq_along(age_bin), age_bin)),
    list(name = "surgical", groups = split(seq_along(surg),    surg)),
    list(name = "icd",      groups = split(seq_along(icd_label), icd_label))
  )

  y <- as.integer(cohort_test$hospital_expire_flag)
  rows <- list()
  for (mod in names(probs)) {
    p <- probs[[mod]]
    for (sp in spec) {
      for (lvl in names(sp$groups)) {
        idx <- sp$groups[[lvl]]
        n   <- length(idx)
        n_ev <- sum(y[idx])
        if (n_ev < n_min_events) next
        cm <- calibration_metrics_point(
          pmax(pmin(p[idx], 1 - 1e-7), 1e-7), y[idx]
        )
        rows[[length(rows) + 1L]] <- data.table::data.table(
          model       = mod,
          subgroup    = sp$name,
          level       = lvl,
          n           = n,
          n_events    = n_ev,
          auroc       = simple_auroc(p[idx], y[idx]),
          ici         = unname(cm["ici"]),
          cal_slope   = unname(cm["slope"])
        )
      }
    }
  }
  data.table::rbindlist(rows)
}

#' Render the headline calibration plot
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Smoothed loess calibration curves per model with the ideal
#' diagonal. Returns a `ggplot` object; save via `ggplot2::ggsave()`.
#'
#' @param calib_list Named list; each element is the output of
#'   [calibration_suite()] for one model.
#' @return A `ggplot` object.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   set.seed(20260517L)
#'   n <- 200L
#'   prob <- stats::plogis(stats::rnorm(n, -2, 1))
#'   y    <- stats::rbinom(n, 1L, prob)
#'   cal  <- calibration_suite(prob, y, n_boot = 25L)
#'   p    <- plot_calibration(list(model_a = cal))
#'   class(p)
#' }
#' @export
plot_calibration <- function(calib_list) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("`ggplot2` is required for plot_calibration.", call. = FALSE)
  }
  curves <- data.table::rbindlist(lapply(names(calib_list), function(nm) {
    d <- calib_list[[nm]]$curve
    if (nrow(d) == 0L) return(NULL)
    data.table::data.table(model = nm, prob = d$prob, observed = d$observed)
  }))
  ggplot2::ggplot(curves,
                  ggplot2::aes(x = prob, y = observed, color = model)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         color = "grey40") +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_color_viridis_d(end = 0.85) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(x = "Predicted probability of in-hospital mortality",
                  y = "Observed proportion (loess)",
                  color = "Model",
                  title  = "Calibration -- PIC v1.1.0 held-out test fold") +
    ggplot2::theme_minimal(base_size = 11)
}

#' Render the headline decision-curve plot
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Net benefit vs threshold across models, with treat-all and
#' treat-none references.
#'
#' @param dca A `data.table` from [decision_curve()] (with `plot_grid =
#'   TRUE`).
#' @return A `ggplot` object.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   set.seed(20260517L)
#'   n <- 200L
#'   p_a <- stats::plogis(stats::rnorm(n, -2, 1))
#'   y   <- stats::rbinom(n, 1L, p_a)
#'   dca <- decision_curve(list(model_a = p_a), y, plot_grid = TRUE)
#'   p   <- plot_decision_curve(dca)
#'   class(p)
#' }
#' @export
plot_decision_curve <- function(dca) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("`ggplot2` is required for plot_decision_curve.", call. = FALSE)
  }
  d <- data.table::copy(dca)
  model_ids <- setdiff(unique(d$model), c("treat_all", "treat_none"))
  ## Role-split palette: model curves on viridis (matches plot_calibration);
  ## treat-all / treat-none on greys so the color channel encodes
  ## "model vs reference" without crowding the model hues.
  model_cols <- viridisLite::viridis(length(model_ids), end = 0.85)
  names(model_cols) <- model_ids
  scale_vals <- c(model_cols, "treat_all" = "grey55", "treat_none" = "grey25")
  ggplot2::ggplot(d, ggplot2::aes(x = threshold, y = net_benefit,
                                    color = model, linetype = type)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey80") +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_color_manual(values = scale_vals, drop = FALSE) +
    ggplot2::scale_linetype_manual(values = c(model = "solid",
                                              all   = "dotdash",
                                              none  = "dotted")) +
    ggplot2::coord_cartesian(xlim = c(0, 0.5),
                             ylim = c(-0.01, max(d$net_benefit) * 1.1)) +
    ggplot2::labs(x = "Decision threshold (probability of mortality)",
                  y = "Net benefit",
                  color = "Model", linetype = "Curve",
                  title  = "Decision curve analysis -- PIC v1.1.0 test fold") +
    ggplot2::theme_minimal(base_size = 11)
}
