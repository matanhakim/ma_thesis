# National priority areas, Government Decision 667 (2013).
#
# The decision defines national priority by sub-district (nafa) and lists
# localities that qualify individually, either as border/threatened
# localities or under other criteria. The three CSV files in
# `data/raw/national_priority/` are the tables of the decision's web page as
# scraped in 2022 (see data/README.md).

#' CBS sub-district (nafa) codes for the 16 sub-districts in the decision's table
nafa_codes <- function() {
  tibble::tribble(
    ~nafa_name, ~nafa_id,
    "גולן", "29",
    "צפת", "21",
    "עכו", "24",
    "באר שבע", "62",
    "כנרת", "22",
    "יזרעאל", "23",
    "יהודה ושומרון", "71",
    "חדרה", "32",
    "ירושלים", "11",
    "אשקלון", "61",
    "חיפה", "31",
    "השרון", "41",
    "רחובות", "44",
    "רמלה", "43",
    "פתח תקווה", "42",
    "תל אביב", "51"
  )
}

#' Read one of the decision's locality tables
#'
#' @param path Path to the CSV (columns: locality code, locality name).
#' @param drop_first_row Whether to drop the first data row. The original
#'   analysis dropped it, so the default reproduces the thesis; see
#'   data/README.md.
#' @return A character vector of four-digit locality ids.
read_priority_localities <- function(path, drop_first_row = TRUE) {
  ids <- read_reference_csv(path)[[1]]
  if (drop_first_row) ids <- ids[-1]
  il.cbs.muni::pad_yishuv_id(ids)
}

#' Determine which local authorities count as national priority areas
#'
#' A locality is a national priority locality if its sub-district is a
#' national priority sub-district or if it is listed individually in either
#' table of the decision. A local authority is a national priority area if
#' more than 75% of its localities are national priority localities, or if at
#' least 50% of its localities appear in the `threshold_list` table (the
#' decision's rule for regional councils with border localities).
#'
#' @param path_nafot CSV of the sub-district table (no header; column 7 is כן/לא).
#' @param path_border CSV of border/threatened localities.
#' @param path_priority CSV of localities listed as national priority.
#' @param yishuv_muni Output of `read_yishuv_muni()`.
#' @param yishuv_nafa Output of `read_yishuv_nafa()`.
#' @param threshold_list Which locality table drives the 50% rule:
#'   `"priority"` reproduces the original analysis, `"border"` follows the
#'   decision's wording literally. See data/README.md.
#' @return A tibble with columns muni_id, is_nat_pri (logical).
read_national_priority <- function(path_nafot, path_border, path_priority,
                                   yishuv_muni, yishuv_nafa,
                                   threshold_list = c("priority", "border")) {
  threshold_list <- match.arg(threshold_list)

  nafot <- readr::read_csv(
    path_nafot, col_names = FALSE,
    col_types = readr::cols(.default = "c"), progress = FALSE
  ) |>
    dplyr::transmute(nafa_name = X1, nafa_nat_pri = X7 == "כן") |>
    dplyr::inner_join(nafa_codes(), dplyr::join_by(nafa_name))
  stopifnot(nrow(nafot) == 16)

  border_ids <- read_priority_localities(path_border)
  priority_ids <- read_priority_localities(path_priority)

  localities <- yishuv_muni |>
    dplyr::left_join(yishuv_nafa, dplyr::join_by(yishuv_id)) |>
    dplyr::left_join(nafot |> dplyr::select(nafa_id, nafa_nat_pri), dplyr::join_by(nafa_id)) |>
    dplyr::mutate(
      nafa_nat_pri = tidyr::replace_na(nafa_nat_pri, FALSE),
      border_nat_pri = yishuv_id %in% border_ids,
      listed_nat_pri = yishuv_id %in% priority_ids,
      is_nat_pri = nafa_nat_pri | border_nat_pri | listed_nat_pri,
      threshold_nat_pri = if (threshold_list == "border") border_nat_pri else listed_nat_pri
    )

  localities |>
    dplyr::filter(!is.na(muni_id)) |>
    dplyr::summarise(
      .by = muni_id,
      is_nat_pri = mean(is_nat_pri) > 0.75 | mean(threshold_nat_pri) >= 0.5
    )
}
