#' Resolve PIC v1.1.0 source paths
#'
#' Returns a named list of canonical paths to PIC CSVs, resolved through
#' the project-local `data_links/pic_v110/` symlink. The package never
#' embeds the Box-synced source path; the symlink is the single point of
#' indirection.
#'
#' @param root Path to the directory containing `data_links/`. Defaults
#'   to the project root (two levels above the package directory when
#'   loaded via `devtools::load_all()`).
#' @param check Logical. If `TRUE` (default), verifies that every
#'   referenced CSV exists and is readable.
#'
#' @return Named list with elements `admissions`, `patients`, `icustays`,
#'   `chartevents`, `labevents`, `diagnoses_icd`, `d_items`,
#'   `d_icd_diagnoses`, `d_labitems`, `prescriptions`, `inputevents`,
#'   `outputevents`, `microbiologyevents`, `surgery_vital_signs`,
#'   `or_exam_reports`, `emr_symptoms`.
#'
#' @export
pic_paths <- function(root = NULL, check = TRUE) {
  root <- root %||% find_project_root()
  base <- fs::path(root, "data_links", "pic_v110")
  files <- list(
    admissions          = "ADMISSIONS.csv",
    patients            = "PATIENTS.csv",
    icustays            = "ICUSTAYS.csv",
    chartevents         = "CHARTEVENTS.csv",
    labevents           = "LABEVENTS.csv",
    diagnoses_icd       = "DIAGNOSES_ICD.csv",
    d_items             = "D_ITEMS.csv",
    d_icd_diagnoses     = "D_ICD_DIAGNOSES.csv",
    d_labitems          = "D_LABITEMS.csv.gz",
    prescriptions       = "PRESCRIPTIONS.csv.gz",
    inputevents         = "INPUTEVENTS.csv.gz",
    outputevents        = "OUTPUTEVENTS.csv.gz",
    microbiologyevents  = "MICROBIOLOGYEVENTS.csv.gz",
    surgery_vital_signs = "SURGERY_VITAL_SIGNS.csv.gz",
    or_exam_reports     = "OR_EXAM_REPORTS.csv.gz",
    emr_symptoms        = "EMR_SYMPTOMS.csv"
  )
  paths <- lapply(files, function(f) fs::path(base, f))
  if (isTRUE(check)) verify_pic_paths(paths)
  paths
}

#' @keywords internal
#' @noRd
verify_pic_paths <- function(paths) {
  missing <- vapply(paths, function(p) !fs::file_exists(p), logical(1))
  if (any(missing)) {
    stop(
      "PIC source files not reachable via `data_links/pic_v110/`.\n",
      "Missing: ", paste(names(paths)[missing], collapse = ", "), "\n",
      "Expected location: ", fs::path_dir(paths[[1]]),
      "\nRun `picMort::ensure_data_links()` (G1) or check the symlink.",
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
