# Budget data: the Ministry of Culture's support payments to organisations
# (Open Budget) and the SELA regulation support to municipalities.

#' Read the Culture Administration's approved supports, by organisation and year
#'
#' Source: Open Budget ("מפתח התקציב"), export of every support approved from
#' the Culture Administration budget items, 2009-2021. Columns used (by
#' position): 6 "מספר תאגיד" (tax id), 7 "שנה" (year), 8 "סה"כ אושר"
#' (approved amount in NIS).
#'
#' @param path Path to the Open Budget CSV export.
#' @return A tibble with columns tax_id, year, budget_approved (NIS).
read_culture_budget <- function(path) {
  readr::read_csv(path, col_types = readr::cols(.default = "c"), progress = FALSE) |>
    dplyr::select(tax_id = 6, year = 7, budget_approved = 8) |>
    dplyr::mutate(
      year = as.integer(year),
      budget_approved = tidyr::replace_na(readr::parse_number(budget_approved), 0)
    ) |>
    dplyr::summarise(.by = c(tax_id, year), budget_approved = sum(budget_approved))
}

#' Read the approved SELA ("סל תרבות עירוני") support to every municipality, 2016-2019
#'
#' Source: Ministry of Culture and Sport, supports to culture bodies
#' 2016-2019, sheet "סל"ע 42-02-56". Two support tracks exist: cultural
#' initiatives ("יוזמות") and festivals ("פסטיבלים"); their sum is the SELA
#' support. Municipalities are identified by tax id and mapped to the CBS id.
#'
#' @param path Path to the ministry's Excel file.
#' @param muni_ids Municipality identifier table from `read_muni_ids()`.
#' @return A tibble with columns year, muni_id, budget_approved_init,
#'   budget_approved_fest, budget_approved_sela (NIS).
read_sela_budget <- function(path, muni_ids) {
  readxl::read_excel(path, sheet = "סל\"ע 42-02-56") |>
    dplyr::slice(-1) |> # first row is the column total
    dplyr::select(
      tax_id = 1,
      init_2016 = 3, fest_2016 = 4, sela_2016 = 5,
      init_2017 = 7, fest_2017 = 8, sela_2017 = 9,
      init_2018 = 11, fest_2018 = 12, sela_2018 = 13,
      init_2019 = 15, fest_2019 = 16, sela_2019 = 17
    ) |>
    dplyr::mutate(tax_id = as.character(tax_id)) |>
    tidyr::pivot_longer(
      !tax_id,
      names_to = c("track", "year"), names_sep = "_",
      values_to = "budget", values_transform = as.numeric
    ) |>
    dplyr::mutate(
      budget = tidyr::replace_na(budget, 0),
      year = as.integer(year)
    ) |>
    tidyr::pivot_wider(
      names_from = track, names_prefix = "budget_approved_", values_from = budget
    ) |>
    dplyr::inner_join(
      muni_ids |> dplyr::select(tax_id, muni_id = cbs_id),
      dplyr::join_by(tax_id)
    ) |>
    dplyr::select(year, muni_id, dplyr::starts_with("budget_approved_"))
}

#' Sum the Culture Administration's supports by local authority and year
#'
#' Every supported organisation is placed in the local authority of its
#' registered locality. Localities that are their own authority carry a
#' two-character id. Supports whose organisation cannot be placed (mostly
#' prizes to individuals) fall into an NA authority and are excluded from the
#' panel.
#'
#' @param culture_budget Output of `read_culture_budget()`.
#' @param organizations Output of `build_organizations()`.
#' @param yishuv_muni Output of `read_yishuv_muni()`.
#' @return A tibble with columns year, muni_id, budget_approved_culture (NIS).
culture_budget_by_muni <- function(culture_budget, organizations, yishuv_muni) {
  culture_budget |>
    dplyr::left_join(
      organizations |> dplyr::select(tax_id, yishuv_id),
      dplyr::join_by(tax_id)
    ) |>
    dplyr::left_join(yishuv_muni, dplyr::join_by(yishuv_id)) |>
    dplyr::mutate(
      muni_id = dplyr::case_when(
        !is.na(muni_id) ~ muni_id,
        stringr::str_length(yishuv_id) == 2 ~ yishuv_id
      )
    ) |>
    dplyr::summarise(
      .by = c(year, muni_id),
      budget_approved_culture = sum(budget_approved)
    )
}

#' Share of the Culture Administration budget that could be placed in a local authority
#'
#' @param culture_by_muni Output of `culture_budget_by_muni()`.
#' @param years Years to report.
#' @return A tibble with columns year, budget_total, budget_placed, share_placed.
culture_budget_coverage <- function(culture_by_muni, years = 2013:2019) {
  culture_by_muni |>
    dplyr::filter(year %in% years) |>
    dplyr::summarise(
      .by = year,
      budget_total = sum(budget_approved_culture),
      budget_placed = sum(budget_approved_culture[!is.na(muni_id)])
    ) |>
    dplyr::mutate(share_placed = budget_placed / budget_total) |>
    dplyr::arrange(year)
}
