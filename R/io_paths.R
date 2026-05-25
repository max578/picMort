#' Resolve PIC v1.1.0 source paths
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Returns a named list of canonical paths to PIC CSVs. If the
#' `PICMORT_DATA_DIR` environment variable is set, it is treated as the
#' directory containing the PIC v1.1.0 CSVs. Otherwise paths are resolved
#' through the project-local `data_links/pic_v110/` symlink.
#'
#' @param root Path to the directory containing `data_links/`. Defaults
#'   to `PICMORT_DATA_DIR` when that environment variable is set, or to
#'   the project root containing `data_links/` otherwise.
#' @param check Logical. If `TRUE` (default), verifies that every
#'   referenced CSV exists and is readable.
#'
#' @return Named list with elements `admissions`, `patients`, `icustays`,
#'   `chartevents`, `labevents`, `diagnoses_icd`, `d_items`,
#'   `d_icd_diagnoses`, `d_labitems`, `prescriptions`, `inputevents`,
#'   `outputevents`, `microbiologyevents`, `surgery_vital_signs`,
#'   `or_exam_reports`, `emr_symptoms`.
#'
#' @examples
#' # Requires the registered PIC v1.1.0 source, either via
#' # `PICMORT_DATA_DIR` or `<project root>/data_links/pic_v110/`.
#' # Run `check = FALSE` to inspect the expected file names without
#' # touching disk.
#' \dontrun{
#' paths <- pic_paths()
#' names(paths)
#' }
#'
#' # File-name discovery without verifying existence
#' tmp <- tempfile(); dir.create(file.path(tmp, "data_links", "pic_v110"),
#'                               recursive = TRUE)
#' paths <- pic_paths(root = tmp, check = FALSE)
#' names(paths)
#' unlink(tmp, recursive = TRUE)
#' @export
pic_paths <- function(root = NULL, check = TRUE) {
  data_dir <- Sys.getenv("PICMORT_DATA_DIR", unset = "")
  if (is.null(root) && nzchar(data_dir)) {
    base <- fs::path_expand(data_dir)
  } else {
    root <- root %||% find_project_root()
    base <- fs::path(root, "data_links", "pic_v110")
  }
  # Each entry lists candidate filenames in preference order. Resolver
  # picks the first candidate that exists AND has non-zero size; falls
  # back to the first candidate (so the verify-step error message can
  # reference a deterministic missing path). Supports mixed .csv / .csv.gz
  # layouts across macOS, Linux, and Windows mirrors of the PIC release.
  files <- list(
    admissions          = c("ADMISSIONS.csv", "ADMISSIONS.csv.gz"),
    patients            = c("PATIENTS.csv", "PATIENTS.csv.gz"),
    icustays            = c("ICUSTAYS.csv", "ICUSTAYS.csv.gz"),
    chartevents         = c("CHARTEVENTS.csv", "CHARTEVENTS.csv.gz"),
    labevents           = c("LABEVENTS.csv", "LABEVENTS.csv.gz"),
    diagnoses_icd       = c("DIAGNOSES_ICD.csv", "DIAGNOSES_ICD.csv.gz"),
    d_items             = c("D_ITEMS.csv", "D_ITEMS.csv.gz"),
    d_icd_diagnoses     = c("D_ICD_DIAGNOSES.csv", "D_ICD_DIAGNOSES.csv.gz"),
    d_labitems          = c("D_LABITEMS.csv", "D_LABITEMS.csv.gz"),
    prescriptions       = c("PRESCRIPTIONS.csv", "PRESCRIPTIONS.csv.gz"),
    inputevents         = c("INPUTEVENTS.csv", "INPUTEVENTS.csv.gz"),
    outputevents        = c("OUTPUTEVENTS.csv", "OUTPUTEVENTS.csv.gz"),
    microbiologyevents  = c("MICROBIOLOGYEVENTS.csv", "MICROBIOLOGYEVENTS.csv.gz"),
    surgery_vital_signs = c("SURGERY_VITAL_SIGNS.csv", "SURGERY_VITAL_SIGNS.csv.gz"),
    or_exam_reports     = c("OR_EXAM_REPORTS.csv", "OR_EXAM_REPORTS.csv.gz"),
    emr_symptoms        = c("EMR_SYMPTOMS.csv", "EMR_SYMPTOMS.csv.gz")
  )
  paths <- lapply(files, function(candidates) {
    candidate_paths <- fs::path(base, candidates)
    exists <- fs::file_exists(candidate_paths)
    non_empty <- exists & fs::file_info(candidate_paths)$size > 0
    if (any(non_empty)) {
      return(candidate_paths[which(non_empty)[1L]])
    }
    candidate_paths[1L]
  })
  if (isTRUE(check)) verify_pic_paths(paths)
  paths
}

#' @keywords internal
#' @noRd
verify_pic_paths <- function(paths) {
  unreadable <- vapply(paths, function(p) {
    !fs::file_exists(p) || fs::file_info(p)$size <= 0
  }, logical(1))
  if (any(unreadable)) {
    stop(
      "PIC source files not reachable via `data_links/pic_v110/`.\n",
      "Missing or empty: ", paste(names(paths)[unreadable], collapse = ", "), "\n",
      "Expected location: ", fs::path_dir(paths[[1]]),
      "\nSet `PICMORT_DATA_DIR` or check the `data_links/pic_v110/` symlink.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
find_project_root <- function() {
  here <- tryCatch(rprojroot::find_root(rprojroot::has_file("DESCRIPTION")),
                   error = function(e) NULL)
  if (!is.null(here)) return(fs::path_dir(here))
  getwd()
}

#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a
