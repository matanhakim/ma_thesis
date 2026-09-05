# Integration checks on the built pipeline. They read the targets store, so
# they are skipped until targets::tar_make() has run.

skip_if_no_store <- function() {
  testthat::skip_if_not(dir.exists(here::here("_targets")), "run targets::tar_make() first")
}

test_that("the panel covers 255 local authorities in each of 7 years without gaps", {
  skip_if_no_store()
  panel <- targets::tar_read(panel, store = here::here("_targets"))
  expect_equal(nrow(panel), 255 * 7)
  expect_equal(sort(unique(panel$year)), 2013:2019)
  expect_false(anyNA(panel$pop))
  expect_false(anyNA(panel$budget_approved_culture))
  expect_false(anyNA(panel$peri_2004_c))
  expect_false(anyNA(panel$is_nat_pri))
  # population is in persons, not thousands (the smallest authority, Harish, had 1,000 residents in 2013)
  expect_true(all(panel$pop >= 1000))
})

test_that("the hypothetical budget redistributes exactly the SELA budget", {
  skip_if_no_store()
  panel <- targets::tar_read(panel, store = here::here("_targets"))
  by_year <- panel |>
    dplyr::summarise(
      .by = year,
      real = sum(budget_approved_culture),
      hypo = sum(budget_approved_culture_hypo),
      sela = sum(budget_approved_sela)
    )
  expect_equal(by_year$real, by_year$hypo)
  expect_equal(by_year$sela[by_year$year < 2016], rep(0, 3))
})

test_that("headline results of the thesis are reproduced", {
  skip_if_no_store()
  store <- here::here("_targets")
  totals <- targets::tar_read(budget_totals, store = store)
  expect_equal(round(totals$budget_per_capita, 1), c(62.0, 62.7, 65.1, 72.1, 80.0, 78.6, 81.9))

  ineq <- targets::tar_read(inequality, store = store) |> dplyr::filter(type == "real")
  expect_equal(round(100 * ineq$top10_pct, 1), c(60.5, 61.0, 59.1, 54.0, 55.3, 52.3, 51.9))
  expect_equal(round(100 * ineq$bot50_pct, 1), c(4.1, 3.9, 4.1, 7.9, 7.9, 8.3, 7.6))

  sela <- targets::tar_read(sela_2018, store = store)
  expect_equal(round(mean(sela$budget_elig_tot)), 187851)
  expect_equal(nrow(sela), 255)
  expect_equal(as.vector(table(sela$sector)), c(85, 170))

  models <- targets::tar_read(sela_models, store = store)
  b <- coef(models$m2)
  expect_equal(round(b[["sectorjewish:elec_likud_pct"]]), 1854)
  expect_equal(round(b[["(Intercept)"]]), 334921)
  expect_equal(round(models$anova$F[2], 1), 14.4)
})
