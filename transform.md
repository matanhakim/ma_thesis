Thesis - Tranform Variables
================

- <a href="#select-and-name-all-relevant-variables"
  id="toc-select-and-name-all-relevant-variables"><span
  class="toc-section-number">1</span> Select and name all relevant
  variables</a>
- <a href="#transform-types-and-problematic-values-of-variables"
  id="toc-transform-types-and-problematic-values-of-variables"><span
  class="toc-section-number">2</span> Transform types and problematic
  values of variables</a>
  - <a href="#change-relevant-charachter-variables-to-numeric"
    id="toc-change-relevant-charachter-variables-to-numeric"><span
    class="toc-section-number">2.1</span> Change relevant charachter
    variables to numeric</a>
  - <a href="#replace-nas-with-0s" id="toc-replace-nas-with-0s"><span
    class="toc-section-number">2.2</span> Replace Na’s with 0’s</a>
  - <a href="#create-factor-variables"
    id="toc-create-factor-variables"><span
    class="toc-section-number">2.3</span> Create factor variables</a>
  - <a href="#round-relevant-numeric-variables"
    id="toc-round-relevant-numeric-variables"><span
    class="toc-section-number">2.4</span> Round relevant numeric
    variables</a>
- <a href="#explore-and-transform-variables"
  id="toc-explore-and-transform-variables"><span
  class="toc-section-number">3</span> Explore and transform variables</a>
  - <a href="#district" id="toc-district"><span
    class="toc-section-number">3.1</span> District</a>
  - <a href="#type" id="toc-type"><span
    class="toc-section-number">3.2</span> type</a>
  - <a href="#population-pop" id="toc-population-pop"><span
    class="toc-section-number">3.3</span> Population (pop)</a>
  - <a href="#age-distribution" id="toc-age-distribution"><span
    class="toc-section-number">3.4</span> Age distribution</a>
  - <a href="#dependance-ratio" id="toc-dependance-ratio"><span
    class="toc-section-number">3.5</span> Dependance ratio</a>
  - <a
    href="#percent-of-immigrants-that-came-to-israel-after-1990-from-the-population"
    id="toc-percent-of-immigrants-that-came-to-israel-after-1990-from-the-population"><span
    class="toc-section-number">3.6</span> Percent of immigrants that came to
    Israel after 1990 from the population</a>
  - <a href="#unemployment-allowance-percent"
    id="toc-unemployment-allowance-percent"><span
    class="toc-section-number">3.7</span> Unemployment Allowance Percent</a>
  - <a href="#income" id="toc-income"><span
    class="toc-section-number">3.8</span> Income</a>
  - <a href="#education" id="toc-education"><span
    class="toc-section-number">3.9</span> Education</a>
  - <a href="#cbs-clusters-and-indexes"
    id="toc-cbs-clusters-and-indexes"><span
    class="toc-section-number">3.10</span> CBS clusters and indexes</a>
  - <a href="#voting" id="toc-voting"><span
    class="toc-section-number">3.11</span> Voting</a>
  - <a href="#budget" id="toc-budget"><span
    class="toc-section-number">3.12</span> Budget</a>

# Select and name all relevant variables

``` r
df <- df %>% 
  select(
    name = 1,
    muni_id,
    district = 3,
    type = 4,
    distance_ta = 5,
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
    ses_c = 231,
    ses_i = 232,
    ses_r = 233,
    peri_c = 237,
    peri_i = 238,
    peri_r = 239,
    last_col(5:0)
  )
```

    Error in df %>% select(name = 1, muni_id, district = 3, type = 4, distance_ta = 5, : could not find function "%>%"

# Transform types and problematic values of variables

## Change relevant charachter variables to numeric

``` r
df <- df %>% 
  mutate(
    across(
      c(
        distance_ta,
        jew_pct,
        arab_pct,
        muslim_pct,
        christ_pct,
        druze_pct,
        immig_1990_pct,
      ),
      as.numeric
    )
  )
```

    Error in df %>% mutate(across(c(distance_ta, jew_pct, arab_pct, muslim_pct, : could not find function "%>%"

``` r
# Check which variables have NA's after the coercion to numeric variable
check <- df %>% 
  summarise(
    across(
      everything(),
      ~sum(is.na(.))
    )
  )
```

    Error in df %>% summarise(across(everything(), ~sum(is.na(.)))): could not find function "%>%"

## Replace Na’s with 0’s

except for distance from Tel Aviv, which is NA for regional councils

``` r
df <- df %>% 
  mutate(
    across(
      c(
        jew_pct,
        arab_pct,
        muslim_pct,
        christ_pct,
        druze_pct,
        immig_1990_pct,
      ),
      replace_na, 0
    )
  )
```

    Error in df %>% mutate(across(c(jew_pct, arab_pct, muslim_pct, christ_pct, : could not find function "%>%"

## Create factor variables

``` r
df <- df %>% 
  mutate(
    across(
      c(
        district,
        type
      ),
      as_factor
    )
  )
```

    Error in df %>% mutate(across(c(district, type), as_factor)): could not find function "%>%"

## Round relevant numeric variables

``` r
df <- df %>% 
  mutate(
    across(
      c(
        distance_ta,
        dep_ratio,
        income_wage,
        income_freelance,
        ends_with("pct")
      ),
      round, 1
    )
  )
```

    Error in df %>% mutate(across(c(distance_ta, dep_ratio, income_wage, income_freelance, : could not find function "%>%"

# Explore and transform variables

## District

``` r
df %>% 
  ggplot(aes(district)) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")
```

    Error in df %>% ggplot(aes(district)): could not find function "%>%"

## type

``` r
df %>% 
  ggplot(aes(type)) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")
```

    Error in df %>% ggplot(aes(type)): could not find function "%>%"

## Population (pop)

``` r
df %>% 
  ggplot(aes(pop)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density()
```

    Error in df %>% ggplot(aes(pop)): could not find function "%>%"

``` r
df %>% 
  ggplot(aes(log10(pop))) +
  geom_histogram(aes(y = ..density..)) +
  geom_density()
```

    Error in df %>% ggplot(aes(log10(pop))): could not find function "%>%"

As seen, the population variable is right-skewed. after a log
transformation, the variable is more normally distributed. \## Sector
(Jewish/Arab/Mixed)

``` r
df %>% 
  ggplot(aes(jew_pct)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density()
```

    Error in df %>% ggplot(aes(jew_pct)): could not find function "%>%"

As seen, the distribution of the jewish population has 3 modes, and
therfore we will create a new categorical variable for sector. Currently
the variable is defined as “Arab” if there are more than 50% Arabs,
“Mixed” if there are between 10%-50% Arabs, and “Jewish” otherwise. I
consider making a “special sector” variable of some sort, to also
include Hardei and Druze. Should it be another variable or added to the
sector variable?

``` r
df <- df %>% 
  mutate(
    sector = as_factor(case_when(
      arab_pct > 50 ~ "arab",
      arab_pct > 10 ~ "mixed",
      TRUE ~ "jewish"
    ))
  )
```

    Error in df %>% mutate(sector = as_factor(case_when(arab_pct > 50 ~ "arab", : could not find function "%>%"

``` r
df %>% 
  count(sector)
```

    Error in df %>% count(sector): could not find function "%>%"

## Age distribution

``` r
df %>% 
  ggplot(aes(age_65_plus_pct)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density()
```

    Error in df %>% ggplot(aes(age_65_plus_pct)): could not find function "%>%"

``` r
df %>% 
  ggplot(aes(age_0_17_pct)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density()
```

    Error in df %>% ggplot(aes(age_0_17_pct)): could not find function "%>%"

It is not clear which one is better, but they are both somewhat normally
distributed with some right-skewness. Let’s check if sector has an
impact on the distribution.

``` r
df %>% 
  pivot_longer(c(age_0_17_pct, age_65_plus_pct), names_to = "var", values_to = "value") %>% 
  ggplot(aes(value)) + 
  geom_histogram(aes(y = ..density..)) +
  geom_density() +
  facet_grid(sector ~ var)
```

    Error in df %>% pivot_longer(c(age_0_17_pct, age_65_plus_pct), names_to = "var", : could not find function "%>%"

It seems like the distribution is not too much effected by sector. No
need to give it a special treatment.

## Dependance ratio

``` r
df %>% 
  ggplot(aes(dep_ratio)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density()
```

    Error in df %>% ggplot(aes(dep_ratio)): could not find function "%>%"

The dependence ratio variable is right-skewed and it is possible to
perform a log10 transformation, but as of now it does not seem to add
more useful information to the age distribution variables, so no need
for transformation.

## Percent of immigrants that came to Israel after 1990 from the population

``` r
df %>% 
  ggplot(aes(immig_1990_pct)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density()
```

    Error in df %>% ggplot(aes(immig_1990_pct)): could not find function "%>%"

It seems that the variable is right-skewed, but it is probably due to
the fact that Arab municipalities don’t have lots of immigrants. Let’s
check this hypothesis.

``` r
df %>% 
  ggplot(aes(immig_1990_pct)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density() +
  facet_wrap(~ sector, scales = "free_y")
```

    Error in df %>% ggplot(aes(immig_1990_pct)): could not find function "%>%"

First, it seems that indeed Arab municipalities don’t have immigrants.
This makes it a good candidate for interaction. Second, the variable is
still right-skewed, even only for jewish municipalities. Let’s try log10
transformation.

``` r
df %>% 
  ggplot(aes(log10(immig_1990_pct))) +
  geom_histogram(aes(y = ..density..)) +
  geom_density() +
  facet_wrap(~ sector, scales = "free_y")
```

    Error in df %>% ggplot(aes(log10(immig_1990_pct))): could not find function "%>%"

Since a lot of municipalities, both Arab and Jewish, don’t have
immigrants at all, the variable is not a good candidate for log10
transformation.

## Unemployment Allowance Percent

``` r
df %>% 
  ggplot(aes(unemp_allowance_pct)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density()
```

    Error in df %>% ggplot(aes(unemp_allowance_pct)): could not find function "%>%"

``` r
df %>% 
  arrange(desc(unemp_allowance_pct)) %>% 
  select(name, unemp_allowance_pct)
```

    Error in df %>% arrange(desc(unemp_allowance_pct)) %>% select(name, unemp_allowance_pct): could not find function "%>%"

This does not seem like a good variable to determine true unemployment,
for none of the top municipalities have Arab or Haredi municipalities.

## Income

``` r
df %>% 
  pivot_longer(c(income_wage, income_freelance, min_wage_pct), names_to = "var", values_to = "value") %>% 
  ggplot(aes(value)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density() +
  facet_wrap(~ var, scales = "free")
```

    Error in df %>% pivot_longer(c(income_wage, income_freelance, min_wage_pct), : could not find function "%>%"

It seems that the percent earning under minimum wage is pretty normally
distributed, but average income of wage workers is right-skewed. The
freelance income is less relevant for the much lower number of
freelancers. Let’s try log10 transformation on wage income.

``` r
df %>% 
  ggplot(aes(log10(income_wage))) +
  geom_histogram(aes(y = ..density..)) +
  geom_density()
```

    Error in df %>% ggplot(aes(log10(income_wage))): could not find function "%>%"

This is much better, a log10 transformation is needed for wage income.

## Education

Let’s begin with checking correlation between the three education
variables

``` r
df %>% 
  select(bagrut_pct, bagrut_uni_pct, high_educ_35_55_pct) %>% 
  cor()
```

    Error in df %>% select(bagrut_pct, bagrut_uni_pct, high_educ_35_55_pct) %>% : could not find function "%>%"

It seems that the Bagrut variables are highly correlated, but less so
with the higher education variable. Let’s check it visually.

``` r
df %>% 
  ggplot(aes(bagrut_pct, bagrut_uni_pct)) +
  geom_point()
```

    Error in df %>% ggplot(aes(bagrut_pct, bagrut_uni_pct)): could not find function "%>%"

``` r
df %>% 
  ggplot(aes(bagrut_pct, high_educ_35_55_pct)) +
  geom_point()
```

    Error in df %>% ggplot(aes(bagrut_pct, high_educ_35_55_pct)): could not find function "%>%"

``` r
df %>% 
  ggplot(aes(bagrut_uni_pct, high_educ_35_55_pct)) +
  geom_point()
```

    Error in df %>% ggplot(aes(bagrut_uni_pct, high_educ_35_55_pct)): could not find function "%>%"

The charts reaffirm this understanding. LEt’s examine the distribution
of these variables.

``` r
df %>% 
  pivot_longer(c(bagrut_pct, bagrut_uni_pct, high_educ_35_55_pct), names_to = "var", values_to = "value") %>% 
  ggplot(aes(value)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density() +
  facet_wrap(~ var, scales = "free")
```

    Error in df %>% pivot_longer(c(bagrut_pct, bagrut_uni_pct, high_educ_35_55_pct), : could not find function "%>%"

The bagrut variables are left-skewed while the higher education variable
is more bimodal and somewhat evenly distributed. We will opt to use the
higher education variable.

## CBS clusters and indexes

``` r
df %>% 
  pivot_longer(c(starts_with("ses"), starts_with("peri")), names_to = "var", values_to = "value") %>% 
  ggplot(aes(value)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density() + 
  facet_wrap(~ var, scales = "free")
```

    Error in df %>% pivot_longer(c(starts_with("ses"), starts_with("peri")), : could not find function "%>%"

Obviously, the rank variable is evenly distributed by definition. We
will use the index variable as it is normally distributed and carries
the most information with it.

## Voting

``` r
df %>% 
  pivot_longer(c(likud_pct, coal_pct), names_to = "var", values_to = "value") %>% 
  ggplot(aes(value)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density() +
  facet_wrap(~ var, scales = "free")
```

    Error in df %>% pivot_longer(c(likud_pct, coal_pct), names_to = "var", : could not find function "%>%"

It seems that both voting variables are left skewed. this might be
because of ineraction with sector variable. Let’s visualise this.

``` r
df %>% 
  pivot_longer(c(likud_pct, coal_pct), names_to = "var", values_to = "value") %>% 
  ggplot(aes(value)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density() +
  facet_grid(var ~ sector, scales = "free")
```

    Error in df %>% pivot_longer(c(likud_pct, coal_pct), names_to = "var", : could not find function "%>%"

It seems that indeed the voting variables are almost always very low in
the Arab sector. Also, this suggests that the “mixed” value of the
sector variable should be otherwise calculated if we check interaction
in the model to prevent overfitting. This is because There are only 13
mixed municipalities. Again. it is possible to create a “special sector”
variable alongside Druze and Haredi.

## Budget

The budget variables would probably be right-skewed, as they are very
much affected by the size of the population.

``` r
df %>% 
  pivot_longer(starts_with("budget"), names_to = "var", values_to = "value") %>% 
  ggplot(aes(value)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density() +
  facet_wrap(~ var)
```

    Error in df %>% pivot_longer(starts_with("budget"), names_to = "var", : could not find function "%>%"

This indeed seems like right-skewness. Let’s try to divide the budget by
population.

``` r
df %>% 
  pivot_longer(starts_with("budget"), names_to = "var", values_to = "value") %>% 
  ggplot(aes(value / pop)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density() +
  facet_wrap(~ var)
```

    Error in df %>% pivot_longer(starts_with("budget"), names_to = "var", : could not find function "%>%"

After normalizing by population size the distribution is even more
right-skewed. Let’s try a log10 transformation.

``` r
df %>% 
  pivot_longer(starts_with("budget"), names_to = "var", values_to = "value") %>% 
  ggplot(aes(log10(1 + value / pop))) +
  geom_histogram(aes(y = ..density..)) +
  geom_density() +
  facet_wrap(~ var)
```

    Error in df %>% pivot_longer(starts_with("budget"), names_to = "var", : could not find function "%>%"

This is much better and the seemingly best transformation to use.
important to note that 1 was added to the calculation to prevent log of
0.
