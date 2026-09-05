# Reference tables that link the several identifier systems used for Israeli
# local authorities and localities. These are frozen snapshots kept in
# `data/reference/`; see `data/README.md` for their provenance.

#' Read the municipality identifier table (255 local authorities)
#'
#' One row per local authority as of 2013-2019, with the identifiers used by
#' three agencies: the Central Bureau of Statistics (`cbs_id`, the `muni_id`
#' used throughout this project), the Ministry of Education (`edu_id`) and the
#' Tax Authority (`tax_id`, the id under which a municipality receives
#' government support). Names are given per agency.
#'
#' @param path Path to `data/reference/muni_ids.csv`.
#' @return A tibble with columns cbs_id, cbs_name, edu_id, edu_name, tax_id, tax_name.
read_muni_ids <- function(path) {
  read_reference_csv(path)
}

#' Read the locality-name lookup (every known spelling -> CBS locality id)
#'
#' @param path Path to `data/reference/yishuv_names.csv`.
#' @return A tibble with columns yishuv_id, yishuv_name.
read_yishuv_names <- function(path) {
  read_reference_csv(path)
}

#' Read the manual locality assignment for organisations missing from the registries
#'
#' A handful of supported bodies (universities, orchestras, companies) have no
#' registered address in the Guidestar or Companies Registrar extracts. Their
#' locality was assigned by hand from public records.
#'
#' @param path Path to `data/reference/organizations_manual_yishuv.csv`.
#' @return A tibble with columns tax_id, yishuv_name.
read_manual_org_yishuv <- function(path) {
  read_reference_csv(path) |>
    dplyr::select(tax_id, yishuv_name)
}
