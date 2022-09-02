Thesis - Model
================

- <a href="#load-libraries" id="toc-load-libraries"><span
  class="toc-section-number">1</span> Load libraries</a>
- <a href="#first-varaible-to-start-with"
  id="toc-first-varaible-to-start-with"><span
  class="toc-section-number">2</span> First varaible to start with</a>

# Load libraries

``` r
library(tidyverse)
library(broom)
library(ggfortify)
```

# First varaible to start with

Let’s check which single variable is best to start with.

``` r
df_mdl1 <- df %>% 
  select(-c(name, muni_id, budget_approved_capita_log10)) %>% 
  map(~ lm(df$budget_approved_capita_log10 ~ .x, data = df)) %>% 
  map(glance) %>% 
  map("r.squared") %>% 
  as_tibble() %>% 
  pivot_longer(everything() ,names_to = "var", values_to = "r.squared") %>% 
  arrange(desc(r.squared))
```

    Error in UseMethod("select"): no applicable method for 'select' applied to an object of class "function"

``` r
df_mdl1
```

    Error in eval(expr, envir, enclos): object 'df_mdl1' not found

Looks like the population variable has the best correlation with the
approved SELA budget. Let’s examine how it fairs with linear regression
assumptions.

``` r
mdl1_pop <- lm(df$budget_approved_capita_log10 ~ df$pop_log10, data = df)
```

    Error in model.frame.default(formula = df$budget_approved_capita_log10 ~ : 'data' must be a data.frame, environment, or list

``` r
df %>% 
  ggplot(aes(pop_log10, budget_approved_capita_log10)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
```

    Error in `ggplot()`:
    !   You're passing a function as global data.
      Have you misspelled the `data` argument in `ggplot()`

``` r
# autoplot(mdl1_pop)
```
