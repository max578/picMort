# API stability policy

This document declares the lifecycle ladder for `picMort`'s public API,
using the conventions of the `lifecycle` R package
(<https://lifecycle.r-lib.org/articles/stages.html>). The policy
exists so that downstream users — Measum and the wider paediatric-ICU
methods community — can predict which functions are safe to depend on
and which are still in flux.

## Stages

| Stage | What it means | What it implies for breakages |
|---|---|---|
| **Experimental** | The function works as documented, but the API surface is provisional. Argument names, defaults, return-value structure, and the function name itself may change. | No deprecation cycle is owed for breaking changes; breakages land in any minor release with a `NEWS.md` entry. |
| **Stable** | The function is part of the package's committed API contract. | Breaking changes require a deprecation cycle: announce in a minor release with `lifecycle::deprecate_warn()` plus a clearly-named successor; remove only in the next major release. |
| **Superseded** | A newer function exists; the superseded function still works without warnings but is not the recommended path. | Indefinitely supported; no removal scheduled. |
| **Deprecated** | The function emits `deprecate_warn()` on every call; a `lifecycle::deprecate_soft()` form is used for less-critical cases. | Removed in the next major release at the earliest. |
| **Defunct** | The function is retained as a stub that errors with `deprecate_stop()`. | Removed entirely in the release after defunct. |

## Current state — v0.1.0

**All 22 exported functions are marked `experimental`.** This is the
correct stance for a first release: the package is the methodological
substrate for a 3-paper series (P1 → P2 → P3) where the cohort
specification is frozen but the evaluation and model-fitting surfaces
are likely to be refined as papers go through peer review.

Per the `lifecycle` package convention, the experimental badge is
rendered in roxygen via `\code{lifecycle::badge("experimental")}` at
the top of every export's `@description` block.

## Planned promotions

The audit at `../picMort_cran_audit/audit_report.md` recommends
extracting the cohort-agnostic surface (calibration / DCA / PIM3 /
leakage audit) into a successor package post-P3. The promotion
schedule for the surface that stays in `picMort`:

| Version | Promoted to stable | Rationale |
|---|---|---|
| 0.2.0 | `pic_paths`, `build_cohort`, `audit_no_leakage`, `cohort_attrition`, `assert_cohort_invariants` | The cohort contract has been frozen since v1.1.0 and shared across P1–P3 without modification. Stabilising the cohort layer first signals to downstream consumers that the data-side API is safe to depend on. |
| 0.3.0 | `compute_pim3`, `pim3_risk_group`, `pim3_face_validity` | Stabilises after the second peer-review cycle (P1 R&R) confirms the proxy decisions are accepted by reviewers. |
| 0.4.0 | `calibration_suite`, `decision_curve`, `discrimination_metrics`, `subgroup_performance` | These are the most-used surfaces by external users. Stabilise after at least one external research group reports using them successfully. |
| 1.0.0 | All currently-exported functions stable; remaining model-fitting wrappers (`fit_elastic_net`, `fit_xgboost`, `fit_bayes_horseshoe`, `predict_mortality`) re-evaluated — the audit's recommended de-coupling pattern would move them to a thin pipeline layer at this point. |

The schedule is indicative, not contractual. Actual promotions are
gated on (a) the underlying paper being accepted at peer review and
(b) the absence of API-breaking feedback during the experimental
period.

## Deprecation mechanics

When a stable function is scheduled for change, the deprecation
cycle is:

1. **Minor release N (announcement).** New successor exported. Old
   function emits `lifecycle::deprecate_warn(when = "N.0", what =
   "old_fn()", with = "new_fn()")` on every call. The `NEWS.md`
   entry calls the deprecation out explicitly and links to the
   migration guide section in the relevant article.
2. **Minor release N+x (cleanup window).** Old function still
   present, still warning. Users have at least one minor-release
   cycle to migrate.
3. **Major release N+1.0 (removal).** Old function moved to
   `defunct` stage (`lifecycle::deprecate_stop()`) and then removed
   in the release after.

## Reporting an API concern

If a function's signature, behaviour, or stage feels wrong, file an
issue at <https://github.com/max578/picMort/issues> with the label
`api-stability`. Concerns raised before the planned stabilisation
release for that function will be considered for the stage change
itself.

## References

- `lifecycle` package: <https://lifecycle.r-lib.org/>
- The R Packages book, chapter on lifecycle:
  <https://r-pkgs.org/lifecycle.html>
- `/rpkg` skill invariant I9 (API-stability mechanism), against
  which this policy is benchmarked.
