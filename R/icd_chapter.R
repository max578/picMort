#' Map an ICD-10 code to its WHO chapter
#'
#' Vectorised. Accepts either ICD-10 codes with a decimal point (e.g.
#' `"K52.901"`, `"J18.900"`) or without (e.g. `"K52901"`). The chapter
#' is determined by the leading letter and the two leading digits per
#' the ICD-10 chapter ranges.
#'
#' @param code Character vector of ICD-10 codes. `NA` codes return `NA`.
#' @return Character vector of chapter names; one of:
#'   `"infectious"`, `"neoplasms"`, `"blood"`, `"endocrine"`, `"mental"`,
#'   `"nervous"`, `"eye"`, `"ear"`, `"circulatory"`, `"respiratory"`,
#'   `"digestive"`, `"skin"`, `"musculoskeletal"`, `"genitourinary"`,
#'   `"pregnancy"`, `"perinatal"`, `"congenital"`, `"symptoms"`,
#'   `"injury"`, `"external"`, `"factors"`, `"special"`, `"unknown"`.
#' @export
icd10_to_chapter <- function(code) {
  out <- rep(NA_character_, length(code))
  ok <- !is.na(code) & nzchar(code)
  if (!any(ok)) return(out)
  letter <- toupper(substr(code[ok], 1L, 1L))
  num    <- suppressWarnings(as.integer(substr(code[ok], 2L, 3L)))

  ch <- character(sum(ok))
  ch[letter == "A" | letter == "B"] <- "infectious"
  ch[(letter == "C") | (letter == "D" & num <= 49)] <- "neoplasms"
  ch[letter == "D" & num >= 50 & num <= 89]         <- "blood"
  ch[letter == "E"]                                 <- "endocrine"
  ch[letter == "F"]                                 <- "mental"
  ch[letter == "G"]                                 <- "nervous"
  ch[letter == "H" & num <= 59]                     <- "eye"
  ch[letter == "H" & num >= 60]                     <- "ear"
  ch[letter == "I"]                                 <- "circulatory"
  ch[letter == "J"]                                 <- "respiratory"
  ch[letter == "K"]                                 <- "digestive"
  ch[letter == "L"]                                 <- "skin"
  ch[letter == "M"]                                 <- "musculoskeletal"
  ch[letter == "N"]                                 <- "genitourinary"
  ch[letter == "O"]                                 <- "pregnancy"
  ch[letter == "P"]                                 <- "perinatal"
  ch[letter == "Q"]                                 <- "congenital"
  ch[letter == "R"]                                 <- "symptoms"
  ch[letter == "S" | letter == "T"]                 <- "injury"
  ch[letter == "V" | letter == "W" |
     letter == "X" | letter == "Y"]                 <- "external"
  ch[letter == "Z"]                                 <- "factors"
  ch[letter == "U"]                                 <- "special"
  ch[!nzchar(ch)]                                   <- "unknown"

  out[ok] <- ch
  out
}
