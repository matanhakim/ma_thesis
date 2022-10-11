Thesis - Model
================

- <a href="#load-libraries" id="toc-load-libraries"><span
  class="toc-section-number">1</span> Load libraries</a>
- <a href="#selecting-relevant-variables"
  id="toc-selecting-relevant-variables"><span
  class="toc-section-number">2</span> Selecting relevant variables</a>
- <a href="#checking-eligibility-for-both-sela-budget-types"
  id="toc-checking-eligibility-for-both-sela-budget-types"><span
  class="toc-section-number">3</span> Checking eligibility for both SELA
  budget types</a>
  - <a href="#add-variables-for-eligibilty"
    id="toc-add-variables-for-eligibilty"><span
    class="toc-section-number">3.1</span> Add variables for eligibilty</a>

After failing to come up with meaningful results in model-01 attempt
with a linear regression model predicting approved budget, we will now
examine the different parameters correlated with varying degrees of SELA
budget usage.

# Load libraries

``` r
library(tidyverse)
library(tidymodels)
library(modelr)
library(corrr)
library(ggfortify)
```

# Selecting relevant variables

``` r
sela_df <- df %>% 
  select(
    name,
    muni_id,
    district,
    type,
    pop,
    likud_pct,
    coal_pct,
    ses_2013_c,
    peri_2004_c,
    sa_data,
    sector,
    is_nat_pri,
    starts_with("budget")
  )
```

    Error in UseMethod("select"): no applicable method for 'select' applied to an object of class "function"

# Checking eligibility for both SELA budget types

## Add variables for eligibilty

``` r
sela_df <- sela_df %>% 
  mutate(
    is_elig_fest = case_when(
      pop > 100000 ~ FALSE,
      ses_2013_c > 7 ~ FALSE,
      ses_2013_c < 7 ~ TRUE,
      type == "מועצה אזורית" & peri_2004_c <= 2 ~ TRUE,
      is_nat_pri ~ TRUE,
      TRUE ~ FALSE
    )
  )
```

    Error in mutate(., is_elig_fest = case_when(pop > 1e+05 ~ FALSE, ses_2013_c > : object 'sela_df' not found

``` r
sela_df %>% 
  group_by(is_elig_fest) %>% 
  summarise(sum = sum(budget_approved_fest))
```

    Error in group_by(., is_elig_fest): object 'sela_df' not found

``` r
check <- sela_df %>% 
  filter(!is_elig_fest & budget_approved_fest > 0)
```

    Error in filter(., !is_elig_fest & budget_approved_fest > 0): object 'sela_df' not found
