# Population-weighted inequality measures of the culture budget per capita.
#
# Every resident of a local authority is assumed to receive the authority's
# culture budget per capita. The measures below therefore weight each
# authority by its population, which is equivalent to (but far faster than)
# expanding the data to one row per resident.

#' Population-weighted Gini coefficient
#'
#' Twice the area between the line of equality and the Lorenz curve, with
#' the Lorenz curve drawn through the cumulative population shares
#' (trapezoidal rule). This is the estimator of `reldist::gini()`, which the
#' thesis used; it is reimplemented here to avoid that package's heavy
#' dependencies. Weights can be any positive numbers.
#'
#' @param x Numeric vector of values (budget per capita).
#' @param w Numeric vector of weights (population).
#' @return A number between 0 and 1.
gini_weighted <- function(x, w) {
  ord <- order(x)
  x <- x[ord]
  w <- w[ord] / sum(w)
  p <- cumsum(w) # cumulative share of the population
  nu <- cumsum(w * x) # cumulative share of the total
  nu <- nu / nu[length(nu)]
  n <- length(nu)
  sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1])
}

#' Share of the total held by the top `prop` of the weighted population
#'
#' Residents are ranked by their value; the top `floor(prop * sum(w))`
#' residents are taken, splitting the authority that straddles the boundary
#' proportionally. This reproduces exactly `uncount(w) |> slice_head(prop)`
#' on integer weights.
#'
#' @param x Numeric vector of values (budget per capita).
#' @param w Numeric vector of integer weights (population).
#' @param prop Share of the population to take from the top (e.g. 0.1).
#' @return The share (0-1) of `sum(x * w)` received by that group.
top_share <- function(x, w, prop) {
  ord <- order(x, decreasing = TRUE)
  x <- x[ord]
  w <- w[ord]
  n_top <- floor(prop * sum(w))
  cum_w <- cumsum(w)
  full <- cum_w <= n_top
  partial <- which(!full)[1]
  total_top <- sum(x[full] * w[full])
  if (!is.na(partial)) {
    total_top <- total_top + (n_top - sum(w[full])) * x[partial]
  }
  total_top / sum(x * w)
}

#' Inequality measures of the culture budget per capita, by year
#'
#' Computes, for the actual budget and for the hypothetical budget without
#' SELA, the weighted Gini coefficient and the shares of the top 10%, the
#' bottom 50% and the deciles 6-9 of residents.
#'
#' @param panel Output of `add_hypothetical_budget()`.
#' @return A long tibble with columns year, type ("real"/"hypo"), gini,
#'   top10_pct, bot50_pct, mid50_90_pct.
inequality_by_year <- function(panel) {
  measures <- function(x, w) {
    top10 <- top_share(x, w, 0.1)
    bot50 <- 1 - top_share(x, w, 0.5)
    tibble::tibble(
      gini = gini_weighted(x, w),
      top10_pct = top10,
      bot50_pct = bot50,
      mid50_90_pct = 1 - top10 - bot50
    )
  }
  panel |>
    dplyr::mutate(pop_int = round(pop)) |>
    dplyr::reframe(
      .by = year,
      dplyr::bind_rows(
        real = measures(budget_approved_culture_per_capita, pop_int),
        hypo = measures(budget_approved_culture_hypo_per_capita, pop_int),
        .id = "type"
      )
    ) |>
    dplyr::mutate(type = factor(type, levels = c("real", "hypo")))
}

#' Culture budget per capita by year and a grouping variable
#'
#' @param panel Output of `add_hypothetical_budget()`.
#' @param group Grouping column(s), tidy-selected: sector, muni_type, or clusters.
#' @return A long tibble with columns year, group, type ("real"/"hypo"),
#'   budget_per_capita (NIS).
budget_per_capita_by <- function(panel, group) {
  panel |>
    dplyr::summarise(
      .by = c(year, {{ group }}),
      real = sum(budget_approved_culture) / sum(pop),
      hypo = sum(budget_approved_culture_hypo) / sum(pop)
    ) |>
    tidyr::pivot_longer(c(real, hypo), names_to = "type", values_to = "budget_per_capita") |>
    dplyr::mutate(type = factor(type, levels = c("real", "hypo")))
}

#' Culture budget per capita by year and collapsed CBS cluster, for both indices
#'
#' @param panel Output of `add_hypothetical_budget()`.
#' @return A long tibble with columns year, cluster_type ("ses_2013_c" /
#'   "peri_2015_c"), cluster_value (collapsed factor), type, budget_per_capita.
budget_per_capita_by_cluster <- function(panel) {
  panel |>
    tidyr::pivot_longer(
      c(ses_2013_c, peri_2015_c),
      names_to = "cluster_type", values_to = "cluster_value"
    ) |>
    dplyr::mutate(cluster_value = collapse_clusters(cluster_value)) |>
    budget_per_capita_by(c(cluster_type, cluster_value))
}

#' Total culture budget and budget per capita, by year
#'
#' @param panel Output of `build_panel()`.
#' @return A tibble with columns year, budget_total, budget_per_capita.
budget_totals_by_year <- function(panel) {
  panel |>
    dplyr::summarise(
      .by = year,
      budget_total = sum(budget_approved_culture),
      budget_per_capita = sum(budget_approved_culture) / sum(pop)
    )
}
