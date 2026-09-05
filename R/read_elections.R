# Results of the elections to the 20th Knesset (March 2015), by locality.

#' Aggregate the 2015 election results to local authorities
#'
#' Sums the votes of every locality in a local authority and computes the
#' share of valid votes cast for the Likud party (list letters "מחל"),
#' rounded to one decimal as in the thesis. Localities that belong to no local
#' authority (unrecognised villages, Mikveh Israel, the Jewish settlement in
#' Hebron, etc.) are dropped. A local authority without any polling station
#' (Ein Qiniyye in 2015) is later given a share of 0.
#'
#' @param path Path to the Central Elections Committee results file.
#' @param yishuv_muni Locality -> local authority mapping from `read_yishuv_muni()`.
#' @return A tibble with columns muni_id, elec_likud_votes, elec_good_votes
#'   (valid votes), elec_pot_votes (eligible voters), elec_likud_pct.
read_elections_2015 <- function(path, yishuv_muni) {
  readxl::read_excel(path) |>
    dplyr::select(
      yishuv_id = "סמל ישוב",
      pot_votes = "בזב",
      good_votes = "כשרים",
      likud_votes = "מחל"
    ) |>
    dplyr::mutate(yishuv_id = il.cbs.muni::pad_yishuv_id(as.character(yishuv_id))) |>
    dplyr::left_join(yishuv_muni, dplyr::join_by(yishuv_id)) |>
    dplyr::filter(!is.na(muni_id)) |>
    dplyr::summarise(
      .by = muni_id,
      elec_likud_votes = sum(likud_votes),
      elec_good_votes = sum(good_votes),
      elec_pot_votes = sum(pot_votes)
    ) |>
    dplyr::mutate(
      elec_likud_pct = round(100 * elec_likud_votes / elec_good_votes, digits = 1)
    )
}
