# Eligibility rules of the 2018 SELA support tests, checked on synthetic authorities.

authority <- function(pop_2015, ses = 5, peri = 5, type = "עירייה", nat_pri = FALSE, approved_init = 1) {
  tibble::tibble(
    pop_2015 = pop_2015, ses_2013_c = ses, peri_2004_c = peri,
    muni_type = type, is_nat_pri = nat_pri, budget_approved_init = approved_init
  )
}

test_that("festival eligibility follows population, cluster, periphery and priority rules", {
  data <- dplyr::bind_rows(
    authority(3000, ses = 3), # small, weak: 70,000
    authority(15000, ses = 6), # threshold cluster: 115,000
    authority(50000, ses = 7, type = "מועצה אזורית", peri = 2), # peripheral regional council: 200,000
    authority(50000, ses = 7, nat_pri = TRUE), # national priority: 200,000
    authority(50000, ses = 7), # cluster 7 without other criteria: 0
    authority(50000, ses = 8, nat_pri = TRUE), # cluster 8 is never eligible: 0
    authority(150000, ses = 2) # too large: 0
  )
  out <- calc_sela_eligibility(data)
  expect_equal(out$budget_elig_fest, c(70000, 115000, 200000, 200000, 0, 0, 0))
})

test_that("initiative scores double for weak, peripheral or priority authorities up to 100,000 residents", {
  data <- dplyr::bind_rows(
    authority(5000, ses = 8), # score 1
    authority(5000, ses = 6), # doubled: 2
    authority(80000, ses = 8, peri = 1), # 3 doubled: 6
    authority(120000, ses = 2), # large authorities never double: 4
    authority(600000, ses = 9) # 7
  )
  out <- calc_sela_eligibility(data, init_total = 1000)
  expect_equal(out$score_elig_init, c(1, 2, 6, 4, 7))
  # the total is split in proportion to the scores of authorities that received a grant
  expect_equal(sum(out$budget_elig_init), 1000)
  expect_equal(out$budget_elig_init, 1000 * out$score_elig_init / 20)
})

test_that("authorities without an approved initiatives grant do not enter the denominator", {
  data <- dplyr::bind_rows(authority(5000, ses = 8), authority(5000, ses = 8, approved_init = 0))
  out <- calc_sela_eligibility(data, init_total = 1000)
  expect_equal(out$budget_elig_init, c(1000, 1000))
})

test_that("skewness and excess kurtosis use the sample variance as scale", {
  x <- c(1, 2, 3)
  m <- moments_skew_kurt(x)
  expect_equal(m$skewness, 0)
  expect_equal(m$kurtosis, (2 / 3) / 1 - 3)
  expect_gt(moments_skew_kurt(c(1, 1, 1, 10))$skewness, 0)
})

test_that("the two calibrated initiative totals are the ones documented", {
  expect_equal(sela_init_total("main"), 23907075)
  expect_equal(sela_init_total("sensitivity"), 23959050)
})
