# Assemble the municipality-year panel (255 local authorities x 2013-2019)
# that all analyses draw on.

#' Build the municipality-year panel
#'
#' Starts from the full list of 255 local authorities for every year, so that
#' authorities without any culture support appear with a budget of 0.
#'
#' @param muni_ids Output of `read_muni_ids()`.
#' @param culture_by_muni Output of `culture_budget_by_muni()`.
#' @param sela_budget Output of `read_sela_budget()`.
#' @param population Output of `read_population()`.
#' @param sector Output of `read_sector()`.
#' @param ses_2013 Output of `read_ses_2013()`.
#' @param peri_2004 Output of `read_peri_2004()`.
#' @param muni_type_peri_2015 Output of `read_muni_type_peri_2015()`.
#' @param national_priority Output of `read_national_priority()`.
#' @param elections Output of `read_elections_2015()`.
#' @param years Years covered by the panel.
#' @return A tibble with one row per local authority and year.
build_panel <- function(muni_ids, culture_by_muni, sela_budget, population,
                        sector, ses_2013, peri_2004, muni_type_peri_2015,
                        national_priority, elections, years = 2013:2019) {
  panel <- tidyr::expand_grid(
    year = as.integer(years),
    muni_ids |> dplyr::select(muni_id = cbs_id, muni_name = cbs_name)
  ) |>
    dplyr::left_join(culture_by_muni, dplyr::join_by(year, muni_id)) |>
    dplyr::left_join(sela_budget, dplyr::join_by(year, muni_id)) |>
    dplyr::left_join(population, dplyr::join_by(year, muni_id)) |>
    dplyr::left_join(sector, dplyr::join_by(muni_id)) |>
    dplyr::left_join(ses_2013, dplyr::join_by(muni_id)) |>
    dplyr::left_join(peri_2004, dplyr::join_by(muni_id)) |>
    dplyr::left_join(muni_type_peri_2015, dplyr::join_by(muni_id)) |>
    dplyr::left_join(national_priority, dplyr::join_by(muni_id)) |>
    dplyr::left_join(elections, dplyr::join_by(muni_id))

  stopifnot(
    nrow(panel) == length(years) * nrow(muni_ids),
    !anyNA(panel$pop), !anyNA(panel$sector), !anyNA(panel$ses_2013_c),
    !anyNA(panel$peri_2015_c), !anyNA(panel$muni_type)
  )

  panel |>
    dplyr::mutate(
      # An authority absent from the budget or election data received nothing.
      dplyr::across(
        c(dplyr::starts_with("budget_approved"), dplyr::starts_with("elec_")),
        \(x) tidyr::replace_na(x, 0)
      ),
      budget_approved_culture_per_capita = budget_approved_culture / pop,
      # Six authorities did not exist in 2004; they take their 2015 value.
      peri_2004_i = dplyr::coalesce(peri_2004_i, peri_2015_i),
      peri_2004_r = dplyr::coalesce(peri_2004_r, peri_2015_r),
      peri_2004_c = dplyr::coalesce(peri_2004_c, peri_2015_c),
      muni_type = factor(muni_type, levels = c("עירייה", "מועצה אזורית", "מועצה מקומית"))
    )
}

#' Add the hypothetical culture budget without the SELA regulation
#'
#' For every year, the SELA budget is redistributed among the local
#' authorities in proportion to their share of the rest of the culture
#' budget (all supports that are not SELA). The result is the budget each
#' authority would have received had SELA never existed and its money been
#' spent like every other culture support (thesis, section 3.2.4).
#'
#' @param panel Output of `build_panel()`.
#' @return The panel with columns budget_approved_sela_hypo,
#'   budget_approved_culture_hypo and budget_approved_culture_hypo_per_capita.
add_hypothetical_budget <- function(panel) {
  panel |>
    dplyr::mutate(
      .by = year,
      budget_non_sela = budget_approved_culture - budget_approved_sela,
      budget_approved_sela_hypo = sum(budget_approved_sela) *
        budget_non_sela / sum(budget_non_sela)
    ) |>
    dplyr::mutate(
      budget_approved_culture_hypo = budget_non_sela + budget_approved_sela_hypo,
      budget_approved_culture_hypo_per_capita = budget_approved_culture_hypo / pop
    ) |>
    dplyr::select(!budget_non_sela)
}
