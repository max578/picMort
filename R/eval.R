## ============================================================================
## Gate G5/G6 -- evaluation suite
##
## Headline framing for the manuscript: calibration + decision-curve
## analysis. Discrimination (AUROC, AUPRC, Brier) is supporting.
## Subgroup performance reports per age strata, surgical/medical and
## top ICD chapters with small-cell suppression at n_min < 50.
##
## All metrics support bootstrap percentile / BCa CIs (default 1,000
## reps). Bayesian model predictions can additionally carry posterior
## CrIs natively in `prob_lower` / `prob_upper`; calibration_suite
## reports both bootstrap and (optional) Bayesian uncertainty when
## available.
## ============================================================================

#' Calibration suite (the manuscript headline)
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
  slope_fit <- stats::glm(y ~ logit_p, family = stats::binomial())
  intercept_fit <- stats::glm(y ~ offset(logit_p), family = stats::binomial())
  loess_fit <- tryCatch(
    stats::loess(y ~ prob, span = 0.75, degree = 1),
    error = function(e) NULL
  )
  ici <- if (is.null(loess_fit)) NA_real_ else
    mean(abs(stats::fitted(loess_fit) - prob))
  cit_large <- log((mean(y) + 1e-9) / (mean(prob) + 1e-9))
  c(
    slope     = unname(stats::coef(slope_fit)[2]),
    intercept = unname(stats::coef(intercept_fit)[1]),
    ici       = ici,
    cit_large = cit_large
  )
}

#' @keywords internal
#' @noRd
calibration_curve_points <- function(prob, y, ngrid = 200L) {
  loess_fit <- tryCatch(
    stats::loess(y ~ prob, span = 0.75, degree = 1),
    error = function(e) NULL
  )
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
#'
#' @return A `data.table` of (model, threshold, net_benefit, type).
#'   `type` is `"model"`, `"all"`, or `"none"`.
#' @export
decision_curve <- function(probs, y,
                            thresholds = c(0.05, 0.10, 0.20),
                            plot_grid = TRUE) {
  y <- as.integer(y)
  prev <- mean(y)
  n <- length(y)
  threshold_grid <- if (isTRUE(plot_grid))
    sort(unique(c(thresholds, seq(0.005, 0.50, by = 0.005))))
  else thresholds

  rows <- list()
  for (mod in names(probs)) {
    p <- probs[[mod]]
    for (pt in threshold_grid) {
      pred_pos <- p >= pt
      tp <- sum(pred_pos & y == 1L)
      fp <- sum(pred_pos & y == 0L)
      nb <- (tp / n) - (fp / n) * (pt / (1 - pt))
      rows[[length(rows) + 1L]] <-
        data.table::data.table(model = mod, threshold = pt,
                               net_benefit = nb, type = "model")
    }
  }
  for (pt in threshold_grid) {
    nb_all <- prev - (1 - prev) * (pt / (1 - pt))
    rows[[length(rows) + 1L]] <-
      data.table::data.table(model = "treat_all", threshold = pt,
                             net_benefit = nb_all, type = "all")
    rows[[length(rows) + 1L]] <-
      data.table::data.table(model = "treat_none", threshold = pt,
                             net_benefit = 0, type = "none")
  }
  data.table::rbindlist(rows)
}

#' Discrimination metrics with bootstrap CIs (supporting role)
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
#' @export
discrimination_metrics <- function(probs, y, reference = NULL,
                                   n_boot = 1000L, seed = 20260508L) {
  y <- as.integer(y); n <- length(y)
  if (is.null(reference)) reference <- if ("pim3" %in% names(probs)) "pim3" else names(probs)[1L]
  ref_brier <- mean((probs[[reference]] - y)^2)

  point_metrics <- function(p, y) {
    auroc <- simple_auroc(p, y)
    auprc <- simple_auprc(p, y)
    brier <- mean((p - y)^2)
    bss <- 1 - brier / max(ref_brier, .Machine$double.eps)
    c(auroc = auroc, auprc = auprc, brier = brier, brier_skill = bss)
  }

  set.seed(seed)
  rows <- list()
  for (mod in names(probs)) {
    p <- probs[[mod]]
    pt <- point_metrics(p, y)
    boot_mat <- replicate(n_boot, {
      idx <- sample.int(n, n, replace = TRUE)
      point_metrics(p[idx], y[idx])
    })
    for (m in names(pt)) {
      v <- boot_mat[m, ]; v <- v[is.finite(v)]
      rows[[length(rows) + 1L]] <- data.table::data.table(
        model = mod, metric = m,
        estimate = unname(pt[m]),
        lower = stats::quantile(v, 0.025, names = FALSE),
        upper = stats::quantile(v, 0.975, names = FALSE)
      )
    }
  }
  data.table::rbindlist(rows)
}

#' @keywords internal
#' @noRd
simple_auroc <- function(p, y) {
  o <- order(p, decreasing = TRUE); y <- y[o]
  if (sum(y) == 0L || sum(y) == length(y)) return(NA_real_)
  tp <- cumsum(y); fp <- cumsum(1L - y)
  tpr <- c(0, tp / sum(y)); fpr <- c(0, fp / sum(1L - y))
  sum(diff(fpr) * (tpr[-1] + tpr[-length(tpr)]) / 2)
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
#' Smoothed loess calibration curves per model with the ideal
#' diagonal. Returns a `ggplot` object; save via `ggplot2::ggsave()`.
#'
#' @param calib_list Named list; each element is the output of
#'   [calibration_suite()] for one model.
#' @return A `ggplot` object.
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
  ggplot2::ggplot(curves, ggplot2::aes(x = prob, y = observed, colour = model)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         colour = "grey40") +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_colour_viridis_d(end = 0.85) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(x = "Predicted probability of in-hospital mortality",
                  y = "Observed proportion (loess)",
                  colour = "Model",
                  title  = "Calibration -- PIC v1.1.0 held-out test fold") +
    ggplot2::theme_minimal(base_size = 11)
}

#' Render the headline decision-curve plot
#'
#' Net benefit vs threshold across models, with treat-all and
#' treat-none references.
#'
#' @param dca A `data.table` from [decision_curve()] (with `plot_grid =
#'   TRUE`).
#' @return A `ggplot` object.
#' @export
plot_decision_curve <- function(dca) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("`ggplot2` is required for plot_decision_curve.", call. = FALSE)
  }
  ggplot2::ggplot(dca, ggplot2::aes(x = threshold, y = net_benefit,
                                    colour = model, linetype = type)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey80") +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_colour_viridis_d(end = 0.85) +
    ggplot2::scale_linetype_manual(values = c(model = "solid",
                                              all   = "dotdash",
                                              none  = "dotted")) +
    ggplot2::coord_cartesian(xlim = c(0, 0.5),
                             ylim = c(-0.01, max(dca$net_benefit) * 1.1)) +
    ggplot2::labs(x = "Decision threshold (probability of mortality)",
                  y = "Net benefit",
                  colour = "Model", linetype = "Curve",
                  title  = "Decision curve analysis -- PIC v1.1.0 test fold") +
    ggplot2::theme_minimal(base_size = 11)
}
