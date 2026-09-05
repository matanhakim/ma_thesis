# The SELA regulation ("סל תרבות עירוני", Ministry of Culture and Sport, 2018
# support tests): eligibility of every local authority, regression models of
# the eligibility, and a sensitivity analysis over the socio-economic
# threshold cluster.

#' Calibrated total budget of the SELA initiatives track in 2018
#'
#' The thesis reports (section 3.3.4) that the published total of the
#' initiatives track, 24,038,827 NIS, does not reproduce the grants approved
#' to the municipalities, and that an alternative total was calibrated by
#' trial and error. Two calibrations appear in the original code: the main
#' analysis (Tables 3-4, Figures 17-18) used 23,907,075 NIS, while the
#' sensitivity analysis (Figure 20) used 23,959,050 NIS, the value quoted in
#' the text. Both are kept so that every result of the thesis is reproduced.
#'
#' @param analysis `"main"` or `"sensitivity"`.
#' @return The total in NIS.
sela_init_total <- function(analysis = c("main", "sensitivity")) {
  switch(match.arg(analysis), main = 23907075, sensitivity = 23959050)
}

#' Data for the 2018 SELA analysis
#'
#' The 2018 support tests use the 2015 population (the latest available when
#' the tests were written), so it is attached to the 2018 rows.
#'
#' @param panel Output of `add_hypothetical_budget()`.
#' @return The 2018 rows of the panel with an extra column pop_2015.
build_sela_2018 <- function(panel) {
  panel |>
    dplyr::filter(year == 2018) |>
    dplyr::left_join(
      panel |> dplyr::filter(year == 2015) |> dplyr::select(muni_id, pop_2015 = pop),
      dplyr::join_by(muni_id)
    )
}

#' Compute every local authority's eligibility under the 2018 SELA support tests
#'
#' Festivals track: authorities of up to 100,000 residents are eligible when
#' their socio-economic cluster is at most `ses_threshold`, or (for clusters
#' up to `ses_threshold + 1`) when they are a regional council in
#' peripherality cluster 1-2 or a national priority area. The grant is 70,000,
#' 115,000 or 200,000 NIS by population size.
#'
#' Initiatives track: a score of 1-7 by population size, doubled for
#' authorities of up to 100,000 residents that are a national priority area,
#' in socio-economic cluster at most `ses_threshold`, or in peripherality
#' cluster 1-2. The track's total budget is divided in proportion to the
#' scores of the authorities that actually received an initiatives grant.
#'
#' @param data Output of `build_sela_2018()`.
#' @param ses_threshold Socio-economic threshold cluster (6 in the regulation).
#' @param init_total Total budget of the initiatives track, in NIS. The
#'   published total (24,038,827 NIS) does not reproduce the approved grants,
#'   so the thesis calibrated it by trial and error (section 3.3.4). The
#'   thesis text reports 23,959,050 NIS; its Tables 3-4 and Figures 17-18 were
#'   computed with 23,907,075 NIS and its Figure 20 with 23,959,050 NIS. See
#'   `sela_init_total()`.
#' @return `data` with columns is_elig_fest, budget_elig_fest,
#'   score_elig_init, budget_elig_init, budget_elig_tot.
calc_sela_eligibility <- function(data, ses_threshold = 6, init_total = sela_init_total("main")) {
  data |>
    dplyr::mutate(
      is_elig_fest = dplyr::case_when(
        pop_2015 > 100000 ~ FALSE,
        ses_2013_c >= ses_threshold + 2 ~ FALSE,
        ses_2013_c <= ses_threshold ~ TRUE,
        muni_type == "מועצה אזורית" & peri_2004_c <= 2 ~ TRUE,
        is_nat_pri ~ TRUE,
        .default = FALSE
      ),
      budget_elig_fest = dplyr::case_when(
        !is_elig_fest ~ 0,
        pop_2015 <= 5000 ~ 70000,
        pop_2015 <= 20000 ~ 115000,
        pop_2015 > 20000 ~ 200000
      ),
      score_elig_init = dplyr::case_when(
        pop_2015 <= 10000 ~ 1,
        pop_2015 <= 50000 ~ 2,
        pop_2015 <= 100000 ~ 3,
        pop_2015 <= 150000 ~ 4,
        pop_2015 <= 200000 ~ 5,
        pop_2015 <= 500000 ~ 6,
        pop_2015 > 500000 ~ 7
      ),
      score_elig_init = dplyr::case_when(
        pop_2015 > 100000 ~ score_elig_init,
        is_nat_pri ~ score_elig_init * 2,
        ses_2013_c <= ses_threshold ~ score_elig_init * 2,
        peri_2004_c <= 2 ~ score_elig_init * 2,
        .default = score_elig_init
      ),
      budget_elig_init = init_total * score_elig_init /
        sum(score_elig_init * (budget_approved_init > 0)),
      budget_elig_tot = budget_elig_fest + budget_elig_init
    )
}

#' Fit the two regression models of SELA eligibility (thesis, section 3.3.5)
#'
#' M1 (base): eligibility ~ population + socio-economic cluster +
#' peripherality cluster. M2 (research): M1 + sector x Likud vote share.
#'
#' @param data Output of `calc_sela_eligibility()`.
#' @return A list with elements m1, m2 (lm objects) and anova (their comparison).
fit_sela_models <- function(data) {
  m1 <- lm(budget_elig_tot ~ pop_2015 + ses_2013_c + peri_2004_c, data = data)
  m2 <- lm(
    budget_elig_tot ~ sector:elec_likud_pct + pop_2015 + ses_2013_c + peri_2004_c,
    data = data
  )
  list(m1 = m1, m2 = m2, anova = anova(m1, m2))
}

#' Predicted "loyalty dividend": the difference between M2 and M1 predictions
#'
#' @param models Output of `fit_sela_models()`.
#' @param data The data the models were fitted on.
#' @return `data` with columns fitted_m1, fitted_m2, loyalty_dividend.
loyalty_dividend <- function(models, data) {
  data |>
    dplyr::mutate(
      fitted_m1 = fitted(models$m1),
      fitted_m2 = fitted(models$m2),
      loyalty_dividend = fitted_m2 - fitted_m1
    )
}

#' Sensitivity of the loyalty dividend to the socio-economic threshold cluster
#'
#' Recomputes eligibility and refits M1 and M2 with every cluster 1-10 as the
#' threshold, and extracts the coefficient of the Jewish-sector x Likud vote
#' share interaction.
#'
#' @param sela_2018 Output of `build_sela_2018()`.
#' @param init_total Passed to `calc_sela_eligibility()`.
#' @return A tibble with columns ses_threshold, b_effect, p_effect, p_anova.
sela_sensitivity <- function(sela_2018, init_total = sela_init_total("sensitivity")) {
  one <- function(k) {
    data <- calc_sela_eligibility(sela_2018, ses_threshold = k, init_total = init_total)
    models <- fit_sela_models(data)
    effect <- broom::tidy(models$m2) |>
      dplyr::filter(term == "sectorjewish:elec_likud_pct")
    tibble::tibble(
      ses_threshold = k,
      b_effect = effect$estimate,
      p_effect = effect$p.value,
      p_anova = models$anova[["Pr(>F)"]][2]
    )
  }
  purrr::map(1:10, one) |> purrr::list_rbind()
}

#' Sample skewness and excess kurtosis
#'
#' Third and fourth standardised moments, with the sample variance (n - 1
#' denominator) as the scale. These are the definitions of
#' `timeDate::skewness(method = "moment")` and
#' `timeDate::kurtosis(method = "excess")`, which the thesis used.
#'
#' @param x Numeric vector.
#' @return A list with elements skewness and kurtosis.
moments_skew_kurt <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)
  v <- var(x)
  list(
    skewness = sum((x - m)^3) / n / v^(3 / 2),
    kurtosis = sum((x - m)^4) / n / v^2 - 3
  )
}
