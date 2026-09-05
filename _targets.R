# Reproducible pipeline of the thesis analysis, built with {targets}.
#
# Run the whole pipeline (data import, analysis and the rendered thesis) with:
#   targets::tar_make()
# Inspect it with targets::tar_visnetwork() or targets::tar_manifest().
#
# Every function called here lives in R/ and is documented there. Raw data
# files are declared as file targets so that a changed file invalidates
# exactly the results that depend on it.

library(targets)
library(tarchetypes)

tar_option_set(
  packages = c(
    "dplyr", "tidyr", "purrr", "stringr", "readr", "readxl", "forcats",
    "tibble", "il.cbs.muni", "broom"
  ),
  format = "rds"
)

tar_source()

#' Pick the CBS local-authority file of a given year from a vector of paths
cbs_muni_file <- function(paths, year) {
  paths[stringr::str_detect(basename(paths), as.character(year))]
}

list(
  # ---- Raw data files ------------------------------------------------------
  tar_target(file_muni_ids, "data/reference/muni_ids.csv", format = "file"),
  tar_target(file_yishuv_names, "data/reference/yishuv_names.csv", format = "file"),
  tar_target(file_manual_orgs, "data/reference/organizations_manual_yishuv.csv", format = "file"),
  tar_target(
    files_cbs_muni,
    list.files("data/raw/municipalities", pattern = "\\.xlsx?$", full.names = TRUE),
    format = "file"
  ),
  tar_target(file_cbs_ses_2013, "data/raw/indices/cbs_socioeconomic_index_2013_t2.xls", format = "file"),
  tar_target(file_cbs_peri_2004, "data/raw/indices/cbs_peripherality_index_2004_t2.xls", format = "file"),
  tar_target(file_cbs_yishuvim, "data/raw/yishuvim/cbs_localities_2021_bycode.xlsx", format = "file"),
  tar_target(file_elections, "data/raw/elections/knesset_20_results_by_locality_2015.xls", format = "file"),
  tar_target(file_guidestar_new, "data/raw/organizations/guidestar_monthly_report_2023_05.xlsx", format = "file"),
  tar_target(file_guidestar_old, "data/raw/organizations/guidestar_report_2020_08.xlsx", format = "file"),
  tar_target(file_companies, "data/raw/organizations/companies_registry.csv.gz", format = "file"),
  tar_target(file_culture_budget, "data/raw/budget/culture_administration_supports_2009_2021.csv", format = "file"),
  tar_target(file_sela_budget, "data/raw/budget/moc_supports_by_regulation_2016_2019.xlsx", format = "file"),
  tar_target(file_nafot_priority, "data/raw/national_priority/nafot_priority.csv", format = "file"),
  tar_target(file_yishuvim_border, "data/raw/national_priority/yishuvim_border.csv", format = "file"),
  tar_target(file_yishuvim_priority, "data/raw/national_priority/yishuvim_priority.csv", format = "file"),

  # ---- Reference tables -----------------------------------------------------
  tar_target(muni_ids, read_muni_ids(file_muni_ids)),
  tar_target(yishuv_names, read_yishuv_names(file_yishuv_names)),
  tar_target(manual_orgs, read_manual_org_yishuv(file_manual_orgs)),

  # ---- CBS data -------------------------------------------------------------
  tar_target(population, read_population(files_cbs_muni)),
  tar_target(sector, read_sector(cbs_muni_file(files_cbs_muni, 2019))),
  tar_target(muni_type_peri_2015, read_muni_type_peri_2015(cbs_muni_file(files_cbs_muni, 2016))),
  tar_target(ses_2013, read_ses_2013(file_cbs_ses_2013)),
  tar_target(peri_2004, read_peri_2004(file_cbs_peri_2004)),
  tar_target(yishuv_muni, read_yishuv_muni(file_cbs_yishuvim)),
  tar_target(yishuv_nafa, read_yishuv_nafa(file_cbs_yishuvim)),

  # ---- Elections, organisations, budgets, national priority -----------------
  tar_target(elections, read_elections_2015(file_elections, yishuv_muni)),
  tar_target(amutot, read_amutot(file_guidestar_new, file_guidestar_old)),
  tar_target(companies, read_companies(file_companies)),
  tar_target(
    organizations,
    build_organizations(amutot, companies, municipality_org_names(muni_ids), manual_orgs, yishuv_names)
  ),
  tar_target(culture_budget, read_culture_budget(file_culture_budget)),
  tar_target(culture_by_muni, culture_budget_by_muni(culture_budget, organizations, yishuv_muni)),
  tar_target(culture_coverage, culture_budget_coverage(culture_by_muni)),
  tar_target(sela_budget, read_sela_budget(file_sela_budget, muni_ids)),
  tar_target(
    national_priority,
    read_national_priority(
      file_nafot_priority, file_yishuvim_border, file_yishuvim_priority,
      yishuv_muni, yishuv_nafa
    )
  ),

  # ---- The municipality-year panel ------------------------------------------
  tar_target(
    panel,
    build_panel(
      muni_ids, culture_by_muni, sela_budget, population, sector, ses_2013,
      peri_2004, muni_type_peri_2015, national_priority, elections
    ) |>
      add_hypothetical_budget()
  ),

  # ---- Chapter 4.1-4.2: inequality in the culture budget --------------------
  tar_target(budget_totals, budget_totals_by_year(panel)),
  tar_target(inequality, inequality_by_year(panel)),
  tar_target(bpc_sector, budget_per_capita_by(panel, sector)),
  tar_target(bpc_muni_type, budget_per_capita_by(panel, muni_type)),
  tar_target(bpc_cluster, budget_per_capita_by_cluster(panel)),

  # ---- Chapter 4.3: political aspects of the SELA regulation ----------------
  tar_target(sela_2018, calc_sela_eligibility(build_sela_2018(panel))),
  tar_target(sela_models, fit_sela_models(sela_2018)),
  tar_target(sela_dividend, loyalty_dividend(sela_models, sela_2018)),
  tar_target(sela_sensitivity_by_cluster, sela_sensitivity(build_sela_2018(panel))),

  # ---- The thesis itself: HTML, Word and PDF (Typst) into output/ -----------
  # Rendering is driven by _quarto.yml; the document reads the targets above
  # with tar_read(), so any change upstream re-renders it.
  tar_quarto(thesis, path = "thesis.qmd", quiet = FALSE)
)
