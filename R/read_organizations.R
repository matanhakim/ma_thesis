# Registered locality of every organisation that may receive support from the
# Ministry of Culture: non-profits (Guidestar), companies (Companies
# Registrar), the municipalities themselves, and a manual list.

#' Read the registered locality of every non-profit (amuta) from Guidestar
#'
#' Two Guidestar extracts are combined: a 2023 monthly report and an older
#' 2020 report that still lists organisations since dissolved. When an
#' organisation appears in both, the alphabetically first non-missing locality
#' name is kept.
#'
#' @param path_new Path to the 2023 Guidestar monthly report (data on sheet 2).
#' @param path_old Path to the August 2020 Guidestar report.
#' @return A tibble with columns tax_id, yishuv_name (cleaned).
read_amutot <- function(path_new, path_old) {
  new <- readxl::read_excel(path_new, sheet = 2) |>
    dplyr::select(tax_id = "מספר ארגון", yishuv_name = "עיר רישום")
  old <- readxl::read_excel(path_old) |>
    dplyr::select(tax_id = "מספר ארגון", yishuv_name = "עיר רישום")

  dplyr::bind_rows(new, old) |>
    dplyr::mutate(
      tax_id = as.character(tax_id),
      yishuv_name = clean_yishuv_name(yishuv_name)
    ) |>
    dplyr::arrange(tax_id, yishuv_name) |>
    dplyr::distinct(tax_id, .keep_all = TRUE)
}

#' Read the registered locality of every company from the Companies Registrar snapshot
#'
#' @param path Path to `data/raw/organizations/companies_registry.csv.gz`
#'   (see `data-raw/download_companies.R`).
#' @return A tibble with columns tax_id, yishuv_name (cleaned).
read_companies <- function(path) {
  readr::read_csv(path, col_types = readr::cols(.default = "c"), progress = FALSE) |>
    dplyr::filter(!is.na(city_name)) |>
    dplyr::transmute(
      tax_id = company_id,
      yishuv_name = clean_yishuv_name(city_name)
    )
}

#' Treat every municipality as an organisation located in itself
#'
#' Municipalities receive culture support directly (notably through the SELA
#' regulation). Each of the names an agency uses for a municipality is mapped
#' to the municipality's tax id.
#'
#' @param muni_ids Municipality identifier table from `read_muni_ids()`.
#' @return A tibble with columns tax_id, yishuv_name.
municipality_org_names <- function(muni_ids) {
  muni_ids |>
    dplyr::select(tax_id, cbs_name, edu_name, tax_name) |>
    tidyr::pivot_longer(!tax_id, names_to = NULL, values_to = "yishuv_name") |>
    dplyr::distinct(yishuv_name, .keep_all = TRUE)
}

#' Build the organisation -> locality lookup used to place culture budgets
#'
#' Combines all four sources, keeps one locality per tax id (the
#' alphabetically first non-missing name) and attaches the CBS locality id.
#'
#' @param amutot Output of `read_amutot()`.
#' @param companies Output of `read_companies()`.
#' @param muni_names Output of `municipality_org_names()`.
#' @param manual Output of `read_manual_org_yishuv()`.
#' @param yishuv_names Output of `read_yishuv_names()`.
#' @return A tibble with columns tax_id, yishuv_name, yishuv_id.
build_organizations <- function(amutot, companies, muni_names, manual, yishuv_names) {
  dplyr::bind_rows(amutot, companies, muni_names, manual) |>
    dplyr::arrange(tax_id, yishuv_name) |>
    dplyr::distinct(tax_id, .keep_all = TRUE) |>
    dplyr::left_join(yishuv_names, dplyr::join_by(yishuv_name))
}
