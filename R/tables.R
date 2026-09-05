# Tables 3 and 4 of the thesis, built with {tinytable} so that the same code
# renders to HTML, Word and PDF (Typst).

#' Format a number to a given number of significant figures, keeping every integer digit
#'
#' Mirrors the formatting the thesis tables were produced with: two significant
#' figures for coefficients (so 0.2211 becomes 0.22 and 325,902 stays
#' 325,902), three for test statistics.
#'
#' @param x Numeric vector.
#' @param digits Significant figures.
#' @return Character vector with thousands separators.
fmt_sigfig <- function(x, digits = 2) {
  decimals <- pmax(digits - 1 - floor(log10(abs(x))), 0)
  decimals[!is.finite(decimals)] <- 0
  purrr::map2_chr(x, decimals, \(v, d) format(round(v, d), big.mark = ",", nsmall = d, scientific = FALSE))
}

#' Format a p-value as in the thesis tables
#'
#' @param p Numeric vector of p-values.
#' @param bold_below Bold the value when it is below this level (NULL for never).
#' @return Character vector (Markdown).
fmt_pvalue <- function(p, bold_below = 0.05) {
  out <- dplyr::case_when(
    p < 0.001 ~ "<0.001",
    p < 0.1 ~ formatC(p, format = "f", digits = 3),
    p < 0.2 ~ formatC(p, format = "f", digits = 2),
    .default = formatC(p, format = "f", digits = 1)
  )
  if (!is.null(bold_below)) {
    out <- dplyr::if_else(p < bold_below, paste0("**", out, "**"), out)
  }
  out
}

#' Table 3: distributions of the variables of the SELA analysis
#'
#' @param sela_2018 Output of `calc_sela_eligibility()`.
#' @return A tinytable.
table_descriptives <- function(sela_2018) {
  num <- function(x, digits = 0) format(round(x, digits), big.mark = ",", nsmall = digits)
  cont <- function(x, unit, digits = 0) {
    c(paste0(num(mean(x), digits), " (", unit, ")"), paste0(num(sd(x), digits), " (", unit, ")"))
  }
  n_arab <- sum(sela_2018$sector == "arab")
  n_jewish <- sum(sela_2018$sector == "jewish")
  n <- nrow(sela_2018)
  pct <- function(k) paste0(round(100 * k / n), "%")

  rows <- list(
    c('זכאות כספית לתקנת סל"ע', cont(sela_2018$budget_elig_tot, 'ש"ח')),
    c("מספר תושבים לשנת 2015", cont(sela_2018$pop_2015, "תושבים")),
    c("אשכול חברתי כלכלי לשנת 2013", num(mean(sela_2018$ses_2013_c), 2), num(sd(sela_2018$ses_2013_c), 2)),
    c("אשכול פריפריאליות לשנת 2004", num(mean(sela_2018$peri_2004_c), 2), num(sd(sela_2018$peri_2004_c), 2)),
    c("אחוז הצבעה לליכוד בשנת 2015", cont(sela_2018$elec_likud_pct, "אחוזי הצבעה")),
    c("מגזר", "", ""),
    c("ערבי", as.character(n_arab), pct(n_arab)),
    c("יהודי", as.character(n_jewish), pct(n_jewish)),
    c('סה"כ', paste0("N = ", n), "")
  )
  data <- purrr::map(rows, \(r) tibble::tibble(a = r[1], b = r[2], c = r[3])) |>
    purrr::list_rbind() |>
    rlang::set_names(c("משתנה", "ממוצע", "סטיית תקן"))

  tinytable::tt(data, notes = list(
    "ממוצע: במשתנה קטגוריאלי – מספר תצפיות.",
    "סטיית תקן: במשתנה קטגוריאלי – שכיחות יחסית."
  )) |>
    tinytable::format_tt(escape = TRUE) |>
    tinytable::style_tt(i = c(7, 8), j = 1, indent = 1) |>
    tinytable::style_tt(i = 9, bold = TRUE) |>
    tinytable::style_tt(j = 1, align = "r") |>
    tinytable::style_tt(j = 2:3, align = "c")
}

#' Table 4: the two regression models of SELA eligibility
#'
#' @param sela_models Output of `fit_sela_models()`.
#' @return A tinytable with a column spanner per model.
table_models <- function(sela_models) {
  term_labels <- c(
    "(Intercept)" = "(Intercept)",
    pop_2015 = "מספר תושבים ברשות (אלפים)",
    ses_2013_c = "אשכול חברתי-כלכלי",
    peri_2004_c = "אשכול פריפריאליות",
    "sectorarab:elec_likud_pct" = "מגזר ערבי * אחוז הצבעה לליכוד ברשות",
    "sectorjewish:elec_likud_pct" = "מגזר יהודי * אחוז הצבעה לליכוד ברשות"
  )
  one_model <- function(model) {
    coefs <- broom::tidy(model) |>
      dplyr::transmute(
        term,
        beta = fmt_sigfig(estimate, 2),
        se = fmt_sigfig(std.error, 2),
        t = fmt_sigfig(statistic, 3),
        p = fmt_pvalue(p.value, bold_below = NULL),
        p_num = p.value
      )
    fit <- broom::glance(model)
    dplyr::bind_rows(
      coefs,
      tibble::tibble(
        term = "Adjusted R²", beta = fmt_sigfig(fit$adj.r.squared, 3),
        se = "", t = "", p = "", p_num = NA_real_
      ),
      tibble::tibble(
        term = "F statistic", beta = fmt_sigfig(fit$statistic, 3),
        se = "", t = "", p = fmt_pvalue(fit$p.value, bold_below = NULL), p_num = fit$p.value
      )
    )
  }
  m1 <- one_model(sela_models$m1)
  m2 <- one_model(sela_models$m2)
  data <- m2 |>
    dplyr::select(term) |>
    dplyr::left_join(m1, dplyr::join_by(term)) |>
    dplyr::left_join(m2, dplyr::join_by(term), suffix = c("_m1", "_m2")) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character), \(x) tidyr::replace_na(x, "")),
      term = dplyr::coalesce(term_labels[term], term)
    )
  # Significant p-values (below 0.05) are set in bold, as in the thesis.
  bold_m1 <- which(!is.na(data$p_num_m1) & data$p_num_m1 < 0.05)
  bold_m2 <- which(!is.na(data$p_num_m2) & data$p_num_m2 < 0.05)
  data <- dplyr::select(data, !dplyr::starts_with("p_num"))
  names(data) <- c("Characteristic", rep(c("Beta", "SE¹", "t-statistic", "p-value"), 2))

  tinytable::tt(data, notes = list("¹ SE = Standard Error")) |>
    tinytable::format_tt(escape = TRUE) |>
    tinytable::group_tt(j = list("M1" = 2:5, "M2" = 6:9)) |>
    tinytable::style_tt(j = 1, align = "r") |>
    tinytable::style_tt(j = 2:9, align = "c") |>
    tinytable::style_tt(i = bold_m1, j = 5, bold = TRUE) |>
    tinytable::style_tt(i = bold_m2, j = 9, bold = TRUE)
}
