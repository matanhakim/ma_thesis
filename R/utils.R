# Small helpers shared across the pipeline.

#' Clean a locality (yishuv) name so it can be matched across data sources
#'
#' Removes punctuation, symbols and digits, except the apostrophe, hyphen,
#' parentheses and double quote that are part of many Hebrew locality names
#' (e.g. "תל אביב - יפו", "כפר חב\"ד"), then collapses whitespace.
#'
#' This must stay identical to the cleaning that produced
#' `data/reference/yishuv_names.csv`, which lists every spelling of every
#' locality name found in the organisation registries.
#'
#' @param x Character vector of raw locality names.
#' @return Character vector of cleaned names.
clean_yishuv_name <- function(x) {
  x |>
    stringr::str_remove_all("[[:punct:][:symbol:][:digit:]&&[^'\\-()\"]]") |>
    stringr::str_squish()
}

#' Read a small reference CSV with every column as character
#'
#' Identifiers (municipality, locality and tax ids) carry leading zeros, so
#' none of the reference tables may ever be parsed as numbers.
#'
#' @param path Path to the CSV file.
#' @return A tibble of character columns.
read_reference_csv <- function(path) {
  readr::read_csv(path, col_types = readr::cols(.default = "c"), progress = FALSE)
}

#' Collapse the ten CBS clusters (1-10) into the three groups used in the thesis
#'
#' @param cluster Numeric or character vector of cluster values 1-10.
#' @return A factor with levels "אשכולות 1-3", "אשכולות 4-6", "אשכולות 7-10".
collapse_clusters <- function(cluster) {
  forcats::fct_collapse(
    factor(cluster, levels = as.character(1:10)),
    "אשכולות 1-3" = c("1", "2", "3"),
    "אשכולות 4-6" = c("4", "5", "6"),
    "אשכולות 7-10" = c("7", "8", "9", "10")
  )
}
