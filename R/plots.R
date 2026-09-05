# Figures of the thesis (Figures 5-20). Every function takes the tidy
# summary it plots and returns a ggplot object; the Quarto document adds the
# caption and alt text.
#
# The visual conventions follow the submitted thesis: `theme_minimal()`, the
# David typeface, ggplot2's default hue palette ordered by the last value of
# each series, solid lines for actual values and dotted lines for the
# hypothetical budget without SELA.

#' Typeface used in every figure
#'
#' David, the typeface of the thesis text, when it is installed (Windows);
#' otherwise its free metric-compatible clone David CLM (Culmus, Linux);
#' otherwise the default sans-serif family.
#'
#' @return A font family name.
thesis_font <- function() {
  families <- unique(systemfonts::system_fonts()$family)
  candidates <- c("David", "David CLM")
  found <- candidates[candidates %in% families]
  if (length(found) > 0) found[[1]] else "sans"
}

#' Base theme of every figure
#'
#' @param base_family Font family; see `thesis_font()`.
#' @return A ggplot2 theme.
theme_thesis <- function(base_family = thesis_font()) {
  ggplot2::theme_minimal(base_family = base_family) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = "white", colour = "black")
    )
}

#' Place the legend inside the panel at relative coordinates
#'
#' @param x,y Position within the panel, 0-1.
#' @param horizontal Whether several legends are laid side by side.
#' @return A ggplot2 theme element.
legend_inside <- function(x, y, horizontal = FALSE) {
  ggplot2::theme(
    legend.position = "inside",
    legend.position.inside = c(x, y),
    legend.box = if (horizontal) "horizontal" else "vertical"
  )
}

# ---- Labels -----------------------------------------------------------------

labels_sector <- c(jewish = "רשויות עם רוב יהודי", arab = "רשויות עם רוב ערבי")
labels_hypo <- c(real = "ערך מדד בפועל", hypo = "ערך מדד היפותטי")
labels_decile <- c(
  bot50_pct = "חלקם של 50% התחתונים בתקציב התרבות",
  mid50_90_pct = "חלקם של העשירונים השישי עד התשיעי בתקציב התרבות",
  top10_pct = "חלקו של העשירון העליון בתקציב התרבות"
)
labels_cluster_type <- c(
  peri_2015_c = "אשכול פריפריאליות (2015)",
  ses_2013_c = "אשכול חברתי-כלכלי (2013)"
)
colours_muni_type <- c(
  "עירייה" = "#F8766D",
  "מועצה אזורית" = "#00BA38",
  "מועצה מקומית" = "#619CFF"
)
linetypes_hypo <- c(real = "solid", hypo = "dotted")
lab_year <- "שנה"
lab_budget_per_capita <- 'תקציב מינהל תרבות לתושב (ש"ח)'

# ---- Chapter 4.1 -------------------------------------------------------------

#' Figure 5: total Culture Administration budget and budget per capita, by year
#'
#' @param budget_totals Output of `budget_totals_by_year()`.
plot_budget_totals <- function(budget_totals) {
  budget_totals |>
    tidyr::pivot_longer(c(budget_total, budget_per_capita), names_to = "measure") |>
    dplyr::mutate(
      measure = factor(measure, levels = c("budget_per_capita", "budget_total")),
      label = dplyr::if_else(
        value < 1e6,
        as.character(round(value, 1)),
        paste0(round(value / 1e6), "M")
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(year, value)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_text(ggplot2::aes(label = label), vjust = -1) +
    ggplot2::facet_wrap(
      ggplot2::vars(measure),
      scales = "free_y",
      labeller = ggplot2::labeller(measure = c(
        budget_per_capita = "תקציב מינהל תרבות לתושב",
        budget_total = "תקציב מינהל תרבות כולל"
      ))
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.1))
    ) +
    ggplot2::scale_x_continuous(
      breaks = 2013:2019,
      expand = ggplot2::expansion(mult = c(0.1, 0.1))
    ) +
    ggplot2::labs(x = lab_year, y = 'ש"ח') +
    theme_thesis() +
    ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
}

#' Line chart of budget per capita by year for the groups of one variable
#'
#' Used for Figures 6-8 (actual budget only) and 12-14 (actual and
#' hypothetical budget, distinguished by line type). Series are ordered by
#' their last value so that legend order matches the chart.
#'
#' @param data Output of `budget_per_capita_by()` (long, with a `type` column).
#' @param group Grouping column (unquoted).
#' @param group_labels Optional named vector of legend labels for `group`.
#' @param hypothetical Whether to draw the hypothetical budget as well.
#' @return A ggplot object.
plot_budget_per_capita <- function(data, group, group_labels = ggplot2::waiver(),
                                   hypothetical = FALSE) {
  if (!hypothetical) {
    data <- dplyr::filter(data, type == "real")
  }
  years <- range(data$year)
  data <- data |>
    dplyr::mutate(
      {{ group }} := forcats::fct_reorder2({{ group }}, year, budget_per_capita),
      label = dplyr::if_else(
        year > 2015 | type == "real",
        as.character(round(budget_per_capita, 1)),
        ""
      )
    )

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(year, budget_per_capita, colour = {{ group }}, shape = {{ group }})
  )
  if (hypothetical) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(linetype = type), linewidth = 1) +
      ggplot2::geom_point(size = 3) +
      ggrepel::geom_text_repel(
        ggplot2::aes(label = label),
        vjust = -1, direction = "y", segment.alpha = 0, show.legend = FALSE
      ) +
      ggplot2::scale_linetype_manual(values = linetypes_hypo, labels = labels_hypo)
  } else {
    p <- p +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_text(ggplot2::aes(label = label), vjust = -1, show.legend = FALSE)
  }
  p +
    ggplot2::scale_x_continuous(breaks = years[1]:years[2]) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.1)),
      limits = c(0, NA)
    ) +
    ggplot2::scale_colour_discrete(labels = group_labels) +
    ggplot2::scale_shape_discrete(labels = group_labels) +
    ggplot2::labs(x = lab_year, y = lab_budget_per_capita) +
    theme_thesis()
}

#' Figures 6 and 12: budget per capita by sector
#' @param bpc_sector Output of `budget_per_capita_by(panel, sector)`.
#' @param hypothetical Whether to add the hypothetical budget (Figure 12).
plot_bpc_sector <- function(bpc_sector, hypothetical = FALSE) {
  plot_budget_per_capita(bpc_sector, sector, labels_sector, hypothetical) +
    if (hypothetical) legend_inside(0.20, 0.45) else legend_inside(0.25, 0.4)
}

#' Figures 7 and 13: budget per capita by type of local authority
#' @param bpc_muni_type Output of `budget_per_capita_by(panel, muni_type)`.
#' @param hypothetical Whether to add the hypothetical budget (Figure 13).
plot_bpc_muni_type <- function(bpc_muni_type, hypothetical = FALSE) {
  plot_budget_per_capita(bpc_muni_type, muni_type, hypothetical = hypothetical) +
    legend_inside(0.25, 0.35, horizontal = hypothetical)
}

#' Figures 8 and 14: budget per capita by socio-economic and peripherality cluster
#' @param bpc_cluster Output of `budget_per_capita_by_cluster()`.
#' @param hypothetical Whether to add the hypothetical budget (Figure 14).
plot_bpc_cluster <- function(bpc_cluster, hypothetical = FALSE) {
  plot_budget_per_capita(bpc_cluster, cluster_value, hypothetical = hypothetical) +
    ggplot2::facet_wrap(
      ggplot2::vars(cluster_type),
      labeller = ggplot2::as_labeller(
        rlang::set_names(rtlr::str_rtl(labels_cluster_type), names(labels_cluster_type))
      )
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.1, 0.1))) +
    if (hypothetical) legend_inside(0.8, 0.1, horizontal = TRUE) else legend_inside(0.9, 0.15)
}

#' Figures 9 and 15: population-weighted Gini coefficient of the culture budget, by year
#' @param inequality Output of `inequality_by_year()`.
#' @param hypothetical Whether to add the hypothetical budget (Figure 15).
plot_gini <- function(inequality, hypothetical = FALSE) {
  data <- if (hypothetical) inequality else dplyr::filter(inequality, type == "real")
  p <- ggplot2::ggplot(data, ggplot2::aes(year, gini)) + theme_thesis()
  if (hypothetical) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(linetype = type), linewidth = 1) +
      ggplot2::scale_linetype_manual(values = linetypes_hypo, labels = labels_hypo) +
      legend_inside(0.25, 0.3)
  } else {
    p <- p + ggplot2::geom_line(linewidth = 1)
  }
  p +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_text(ggplot2::aes(label = round(gini, 3)), vjust = -1) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.1, 0.1))) +
    ggplot2::scale_x_continuous(breaks = 2013:2019) +
    ggplot2::labs(x = lab_year, y = "ערך מדד ג'יני")
}

#' Figures 10 and 16: shares of the culture budget by decile group of residents, by year
#' @param inequality Output of `inequality_by_year()`.
#' @param hypothetical Whether to add the hypothetical budget (Figure 16).
plot_decile_shares <- function(inequality, hypothetical = FALSE) {
  data <- inequality |>
    tidyr::pivot_longer(
      c(top10_pct, bot50_pct, mid50_90_pct),
      names_to = "group", values_to = "share"
    ) |>
    dplyr::mutate(
      group = factor(group, levels = names(labels_decile)),
      label = dplyr::if_else(
        year > 2015 | type == "real", scales::percent(share, accuracy = 0.1), ""
      )
    )
  if (!hypothetical) data <- dplyr::filter(data, type == "real")

  p <- ggplot2::ggplot(data, ggplot2::aes(year, share, colour = group, shape = group)) +
    theme_thesis()
  if (hypothetical) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(linetype = type), linewidth = 1) +
      ggplot2::geom_point(size = 3) +
      ggrepel::geom_text_repel(
        ggplot2::aes(label = label),
        vjust = -1, direction = "y", show.legend = FALSE
      ) +
      ggplot2::scale_linetype_manual(values = linetypes_hypo, labels = labels_hypo) +
      legend_inside(0.45, 0.32, horizontal = TRUE)
  } else {
    p <- p +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_text(ggplot2::aes(label = label), vjust = -1, show.legend = FALSE) +
      legend_inside(0.25, 0.3)
  }
  p +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(),
      expand = ggplot2::expansion(mult = c(0, 0.1)),
      limits = c(0, NA)
    ) +
    ggplot2::scale_x_continuous(breaks = 2013:2019) +
    ggplot2::scale_colour_discrete(labels = labels_decile) +
    ggplot2::scale_shape_discrete(labels = labels_decile) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(reverse = TRUE),
      shape = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggplot2::labs(x = lab_year, y = "חלקה של כל קבוצה מתוך תקציב מינהל תרבות")
}

# ---- Chapter 4.2 -------------------------------------------------------------

#' Figure 11: actual versus hypothetical culture budget per capita, 2018
#'
#' Both axes are logarithmic; 0.1 NIS is added to every value so that
#' authorities with no budget can be drawn (thesis, footnote 10).
#'
#' @param panel Output of `add_hypothetical_budget()`.
#' @param year Year to draw.
plot_real_vs_hypo <- function(panel, year = 2018) {
  panel |>
    dplyr::filter(year == !!year) |>
    ggplot2::ggplot(ggplot2::aes(
      log10(budget_approved_culture_per_capita + 0.1),
      log10(budget_approved_culture_hypo_per_capita + 0.1),
      size = pop, colour = muni_type
    )) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_abline() +
    ggplot2::scale_x_continuous(breaks = -1:3, labels = 10^(-1:3)) +
    ggplot2::scale_y_continuous(breaks = -1:3, labels = 10^(-1:3)) +
    ggplot2::scale_colour_manual(values = colours_muni_type) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        override.aes = list(size = 3), title = "סוג רשות מקומית"
      ),
      size = ggplot2::guide_legend(title = "אוכלוסייה")
    ) +
    ggplot2::labs(
      x = rtlr::str_rtl('תקציב תרבות בפועל לתושב (ש"ח, סולם לוגריתמי)'),
      y = rtlr::str_rtl('תקציב תרבות היפותטי לתושב (ש"ח, סולם לוגריתמי)')
    ) +
    theme_thesis() +
    ggplot2::theme(legend.title = ggplot2::element_text()) +
    legend_inside(0.2, 0.8, horizontal = TRUE)
}

# ---- Chapter 4.3 -------------------------------------------------------------

#' Figure 17: distribution of the 2018 SELA eligibility across local authorities
#' @param sela_2018 Output of `calc_sela_eligibility()`.
plot_eligibility_hist <- function(sela_2018) {
  ggplot2::ggplot(sela_2018, ggplot2::aes(budget_elig_tot)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::labs(
      x = 'גובה הזכאות של רשות מקומית לתמיכה תקציבית של תקנת סל"ע בשנת 2018 (ש"ח)',
      y = "מספר רשויות"
    ) +
    theme_thesis()
}

#' Figure 18: predicted loyalty dividend by Likud vote share, by sector
#' @param sela_dividend Output of `loyalty_dividend()`.
plot_loyalty_dividend <- function(sela_dividend) {
  sector_labels <- c(arab = "ערבי", jewish = "יהודי")
  ggplot2::ggplot(
    sela_dividend,
    ggplot2::aes(elec_likud_pct, loyalty_dividend, colour = sector, shape = sector)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1)) +
    ggplot2::scale_colour_discrete(labels = sector_labels) +
    ggplot2::scale_shape_discrete(labels = sector_labels) +
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 3))) +
    ggplot2::labs(
      x = "אחוז הצבעה לליכוד בבחירות 2015",
      # A right-to-left embedding mark (U+202B) keeps the quotation marks in place.
      y = '\u202b"דיבידנד הנאמנות בתרבות" (ש"ח)',
      colour = "מגזר",
      shape = "מגזר"
    ) +
    theme_thesis() +
    ggplot2::theme(
      legend.title = ggplot2::element_text(),
      legend.background = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

#' Figure 19: Likud vote share in 2015 by socio-economic cluster
#'
#' The share is computed from the total votes of all authorities in a cluster.
#'
#' @param panel Output of `build_panel()` (any year; election data is constant).
plot_likud_by_cluster <- function(panel) {
  panel |>
    dplyr::filter(year == 2018) |>
    dplyr::summarise(
      .by = ses_2013_c,
      elec_likud_pct = round(100 * sum(elec_likud_votes) / sum(elec_good_votes), 1)
    ) |>
    dplyr::mutate(ses_2013_c = factor(ses_2013_c, levels = 1:10)) |>
    ggplot2::ggplot(ggplot2::aes(ses_2013_c, elec_likud_pct)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(elec_likud_pct, "%")), vjust = -0.5) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      expand = ggplot2::expansion(mult = c(0, 0.1))
    ) +
    ggplot2::labs(
      x = rtlr::str_rtl("אשכול חברתי-כלכלי (2013)"),
      y = rtlr::str_rtl("אחוז הצבעה לליכוד בבחירות 2015")
    ) +
    theme_thesis()
}

#' Figure 20: sensitivity of the loyalty dividend to the threshold cluster
#'
#' @param sensitivity Output of `sela_sensitivity()`.
#' @param highlight Threshold cluster actually used by the regulation.
plot_sensitivity <- function(sensitivity, highlight = 6) {
  sensitivity |>
    dplyr::mutate(
      stars = dplyr::case_when(
        p_effect > 0.05 ~ "",
        p_effect > 0.01 ~ "*",
        p_effect > 0.001 ~ "**",
        .default = "***"
      ),
      label = paste0(stars, scales::comma(b_effect)),
      highlighted = ses_threshold == highlight
    ) |>
    ggplot2::ggplot(ggplot2::aes(ses_threshold, b_effect, fill = highlighted)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = label, vjust = -1 * sign(b_effect))) +
    ggplot2::scale_x_continuous(breaks = 1:10) +
    ggplot2::scale_y_continuous(
      labels = scales::label_comma(),
      expand = ggplot2::expansion(mult = c(0.1, 0.1))
    ) +
    ggplot2::scale_fill_manual(values = c("TRUE" = "darkred", "FALSE" = "grey35"), guide = "none") +
    ggplot2::labs(
      x = "אשכול סף חברתי-כלכלי",
      y = 'דיבידנד נאמנות (ש"ח)',
      caption = "* p < 0.1, ** p < 0.01, *** p < 0.001"
    ) +
    theme_thesis() +
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0))
}
