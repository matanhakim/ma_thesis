Thesis - Tranform Variables
================

- <a href="#select-and-name-all-relevant-variables"
  id="toc-select-and-name-all-relevant-variables"><span
  class="toc-section-number">1</span> Select and name all relevant
  variables</a>

# Select and name all relevant variables

``` r
df <- df %>% 
  select(
    name = 1,
    district = 3,
    type = 4,
    distance_ta = 5,
    area = 11,
    pop = 13,
    jew_pct = 14,
    arab_pct = 16,
    muslim_pct = 17,
    christ_pct = 18,
    druze_pct = 19,
    age_0_4_pct = 22,
    age_5_9_pct = 23,
    age_10_14_pct = 24,
    age_15_19_pct = 25,
    age_20_29_pct = 26,
    age_30_44_pct = 27,
    age_45_59_pct = 28,
    age_60_64_pct = 29,
    age_65_plus_pct = 30,
    age_0_17_pct = 31,
    age_75_plus_pct = 32,
    dep_ratio = 33,
    immig_1990_pct = 41,
    unemp_allowance_pct = 87,
    income_wage = 116,
    wage_num = 122,
    min_wage_pct = 123,
    freelance_num = 124,
    income_freelance = 125,
    bagrut_pct = 154,
    bagrut_uni_pct = 155,
    high_educ_35_55_pct = 156,
    high_educ_enter_8_pct = 157,
    ses_c = 231,
    ses_i = 232,
    ses_r = 233,
    peri_c = 237,
    peri_i = 238,
    peri_r = 239,
    last_col(5:0)
  )
```

    Error in df %>% select(name = 1, district = 3, type = 4, distance_ta = 5, : could not find function "%>%"