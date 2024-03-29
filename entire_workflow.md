# Thesis - Import Files


- [<span class="toc-section-number">1</span> Setup](#setup)
  - [<span class="toc-section-number">1.1</span> Load
    libraries](#load-libraries)
  - [<span class="toc-section-number">1.2</span> Library
    conflicts](#library-conflicts)
  - [<span class="toc-section-number">1.3</span> Graphics](#graphics)
- [<span class="toc-section-number">2</span> Utility
  functions](#utility-functions)
  - [<span class="toc-section-number">2.1</span> Get file
    extension](#get-file-extension)
  - [<span class="toc-section-number">2.2</span> Read online Excel
    file](#read-online-excel-file)
  - [<span class="toc-section-number">2.3</span> Fix column
    names](#fix-column-names)
  - [<span class="toc-section-number">2.4</span> Fix yishuv
    id](#fix-yishuv-id)
  - [<span class="toc-section-number">2.5</span> Clean yishuv
    name](#clean-yishuv-name)
  - [<span class="toc-section-number">2.6</span> Replace NAs with other
    column](#replace-nas-with-other-column)
  - [<span class="toc-section-number">2.7</span> Plot line chart of a
    statistic by year](#plot-line-chart-of-a-statistic-by-year)
- [<span class="toc-section-number">3</span> Municipalities
  data](#municipalities-data)
  - [<span class="toc-section-number">3.1</span> Import a single
    municipalities file from CBS (2016 and
    later)](#import-a-single-municipalities-file-from-cbs-2016-and-later)
  - [<span class="toc-section-number">3.2</span> Import a single
    variable from a CBS municipalities file (2016 and later) with a
    single
    variable](#import-a-single-variable-from-a-cbs-municipalities-file-2016-and-later-with-a-single-variable)
  - [<span class="toc-section-number">3.3</span> Import a single
    variable from a CBS municipalities file (2015 and before) with a
    single
    variable](#import-a-single-variable-from-a-cbs-municipalities-file-2015-and-before-with-a-single-variable)
  - [<span class="toc-section-number">3.4</span> Municipality
    id](#municipality-id)
  - [<span class="toc-section-number">3.5</span> Yishuvim](#yishuvim)
  - [<span class="toc-section-number">3.6</span> 2013 CBS SES
    data](#2013-cbs-ses-data)
  - [<span class="toc-section-number">3.7</span> 2004 CBS periphery
    data](#2004-cbs-periphery-data)
- [<span class="toc-section-number">4</span> Elections
  data](#elections-data)
- [<span class="toc-section-number">5</span> Organizations
  data](#organizations-data)
  - [<span class="toc-section-number">5.1</span> Amutot](#amutot)
  - [<span class="toc-section-number">5.2</span> Companies](#companies)
  - [<span class="toc-section-number">5.3</span>
    Municipalities](#municipalities)
  - [<span class="toc-section-number">5.4</span> Organiztions with no
    record](#organiztions-with-no-record)
  - [<span class="toc-section-number">5.5</span> Match yishuv id to
    organizations](#match-yishuv-id-to-organizations)
- [<span class="toc-section-number">6</span> Budget data](#budget-data)
  - [<span class="toc-section-number">6.1</span> Sela](#sela)
  - [<span class="toc-section-number">6.2</span> Culture
    (total)](#culture-total)
- [<span class="toc-section-number">7</span> CBS cluster
  data](#cbs-cluster-data)
  - [<span class="toc-section-number">7.1</span> National priority
    settlements decided by the Israeli
    government](#national-priority-settlements-decided-by-the-israeli-government)
    - [<span class="toc-section-number">7.1.1</span> Getting the list of
      tables from the national priority
      webpage](#getting-the-list-of-tables-from-the-national-priority-webpage)
    - [<span class="toc-section-number">7.1.2</span> Merging national
      priority yishuvim, subdistricts (Nafot) and yishuvim close to the
      border](#merging-national-priority-yishuvim-subdistricts-nafot-and-yishuvim-close-to-the-border)
- [<span class="toc-section-number">8</span> Create a complete data
  frame](#create-a-complete-data-frame)
  - [<span class="toc-section-number">8.1</span> Read population for
    every year](#read-population-for-every-year)
  - [<span class="toc-section-number">8.2</span> Classify municipalities
    to sector
    (Jewish/Arab)](#classify-municipalities-to-sector-jewisharab)
  - [<span class="toc-section-number">8.3</span> Read periphery 2015
    indices](#read-periphery-2015-indices)
  - [<span class="toc-section-number">8.4</span> Combine all data
    frames](#combine-all-data-frames)
  - [<span class="toc-section-number">8.5</span> Recode and add
    variables](#recode-and-add-variables)
- [<span class="toc-section-number">9</span> Inequality
  measures](#inequality-measures)
  - [<span class="toc-section-number">9.1</span> Top 10%](#top-10)
  - [<span class="toc-section-number">9.2</span> Gini](#gini)
  - [<span class="toc-section-number">9.3</span> Total
    budget](#total-budget)
  - [<span class="toc-section-number">9.4</span> Inequality
    calculation](#inequality-calculation)
  - [<span class="toc-section-number">9.5</span> Visualize](#visualize)
- [<span class="toc-section-number">10</span> Growth rate of budget by
  group](#growth-rate-of-budget-by-group)
- [<span class="toc-section-number">11</span> Sela
  Analysis](#sela-analysis)
  - [<span class="toc-section-number">11.1</span> Sela 2018
    eligibility](#sela-2018-eligibility)
    - [<span class="toc-section-number">11.1.1</span> Festival
      eligibility](#festival-eligibility)
    - [<span class="toc-section-number">11.1.2</span> Initiatives
      eligibility](#initiatives-eligibility)
  - [<span class="toc-section-number">11.2</span> Modelling Sela
    budget](#modelling-sela-budget)
    - [<span class="toc-section-number">11.2.1</span> Check distribution
      of dependant variable](#check-distribution-of-dependant-variable)
    - [<span class="toc-section-number">11.2.2</span> Distribution table
      of all variables](#distribution-table-of-all-variables)
    - [<span class="toc-section-number">11.2.3</span> Model with Likud
      voting percent](#model-with-likud-voting-percent)
    - [<span class="toc-section-number">11.2.4</span> Check model
      outcomes](#check-model-outcomes)
  - [<span class="toc-section-number">11.3</span> Sensitivity analysis
    for Sela SES cluster
    threshhold](#sensitivity-analysis-for-sela-ses-cluster-threshhold)
    - [<span class="toc-section-number">11.3.1</span> Function for
      calculating eligibilty by SES
      cluster](#function-for-calculating-eligibilty-by-ses-cluster)
    - [<span class="toc-section-number">11.3.2</span> Function for
      modeling by cluster](#function-for-modeling-by-cluster)
    - [<span class="toc-section-number">11.3.3</span> Create data frame
      for model by SES cluster
      threshhold](#create-data-frame-for-model-by-ses-cluster-threshhold)
  - [<span class="toc-section-number">11.4</span> Checking budget per
    Likud voter](#checking-budget-per-likud-voter)

# Setup

## Load libraries

``` r
library(conflicted)
library(tidyverse)
library(readxl)
library(httr)
library(rvest)
library(reldist)
library(scales)
library(timeDate)
library(gtsummary)
library(tidymodels)
library(rtlr)
library(extrafont)
# library(rvest)
# locale("he")
```

## Library conflicts

``` r
conflicts_prefer(dplyr::filter())
```

## Graphics

``` r
theme_set(theme_minimal())
theme_update(
  text = element_text(family = "David")
)
decile_labs <- c(
  "חלקם של 50% התחתונים בתקציב התרבות",
  "חלקם של העשירונים השישי עד התשיעי בתקציב התרבות",
  "חלקו של העשירון העליון בתקציב התרבות"
)

sector_labs <- c(
  "רשויות עם רוב יהודי",
  "רשויות עם רוב ערבי"
  
)
```

# Utility functions

This function finds the file extension of a file. It receives a string
as an argument, and returns the last letters and numbers of the string,
prefixed by a dot (`.`). This helps to identify the file type from a
file path, mainly used to write temporary files to disk when reading
Excel files.

## Get file extension

``` r
get_file_ext <- function(string) {
  str_c(".", str_extract(string, "[0-9a-z]+$"))
}
```

## Read online Excel file

This function reads an Excel file from a url online. It receives a url
and other arguments used by `read_excel()`, and returns the tibble after
being read.

``` r
read_excel_url <- function(url, ...) {
  GET(url, write_disk(tf <- tempfile(fileext = get_file_ext(url))))
  read_excel(tf, ...)
}
```

## Fix column names

This function helps to fix column names of Excel tables, mainly of the
form of those found in Israeli CBS municipality data. It does so by
fixing a single row of to-be column names. It receives a data frame, an
integer number representing a single row considered as holding (some of
the) variable names, and a logical length-one vector specifying whether
to fill missing values with preceding values or not. This last argument
is mostly used in the case of merged cells in Excel files. The function
first transposes the row, and then either fills it with values or turns
NAs to empty strings.

``` r
fix_names <- function(data, row_num, fill_missing) {
  data <- data |> 
    slice(row_num) |> 
    pivot_longer(everything()) |> 
    select(value)
  
  if (fill_missing) {
    data <- data |> 
      fill(value)
  }
  else {
    data <- data |> 
      replace_na(list(value = "")) |> 
      mutate(
        value = if_else(
          str_length(value) > 0,
          str_c(" ", value),
          value
        )
      )
  }
  data |> 
    pull(value)
}
```

This function fixes column names of Excel tables, mainly of the form of
those found in Israeli CBS municipality data. It receives a data frame,
a vector with the row numbers considered as holding the variable names,
and a logical vector specifying whether to fill missing values with
preceding values or not. This last argument should be either of length
1, or of length of `row_num`. It is mostly used in the case of merged
cells in Excel files. The function saves the new column names by
iterating over every row, fixing the variable names, binding all names
to a single tibble, uniting the different columns to a single column and
pulling these values as a vector. The old rows containing these column
names are filtered out, and the new merged and fixed names are set to
the data frame, which is then returned.

``` r
merge_names <- function(data, names_num = 2, fill_missing = TRUE) {
  rows <- seq(names_num)
  col_names_merged <- map2(
    rows,
    fill_missing,
    \(i, fill_missing) fix_names(data, row_num = i, fill_missing)
  ) |> 
    bind_cols() |> 
    unite(col = "var_names", sep = "") |> 
    pull(var_names)

  data <- data |>
    slice(-rows)
  
  names(data) <- col_names_merged
  
  data
}
```

## Fix yishuv id

``` r
fix_yishuv_id <- function(yishuv_id) {
  str_pad(yishuv_id, width = 4, side = "left", pad = "0")
}
```

## Clean yishuv name

``` r
clean_yishuv_name <- function(yishuv_name) {
  yishuv_name |> 
    str_remove_all("[[:punct:][:symbol:][:digit:]&&[^'\\-()\"]]") |> 
    str_squish()
}
```

## Replace NAs with other column

``` r
replace_match <- function(vec, vec_id, match_id, replace_with) {
  case_match(
    {{ vec_id }},
    match_id ~ {{ replace_with }},
    .default = vec
  )
}
```

## Plot line chart of a statistic by year

``` r
plot_line_group <- function(data, group, x = year, y = budget_per_capita, legend_labs = waiver()) {
  min_x <- min(data |> pull({{ x }}))
  max_x <- max(data |> pull({{ x }}))
  
  data |> 
    mutate({{ group }} := fct_reorder2({{ group }}, {{ x }}, {{ y }})) |> 
    ggplot(aes({{ x }}, {{ y }}, color = {{ group }}, shape = {{ group }})) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    geom_text(
      data = data |> 
        filter(year %in% c(max_x, min_x)),
      aes(label = round({{ y }}, digits = 1)),
      vjust = -1,
      show.legend = FALSE
    ) +
    scale_x_continuous(breaks = min_x:max_x) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, NA)) +
    scale_color_discrete(labels = legend_labs) +
    scale_shape_discrete(labels = legend_labs) +
     theme(
      legend.title= element_blank(),
      legend.background = element_rect(fill = "white", color = "black")
    )
}
```

# Municipalities data

## Import a single municipalities file from CBS (2016 and later)

This is a function that gets a path and returns a tibble. First, it
creates a temporary file: it either downloads the file with the path
parameter as url, or uses the local path. later, it reads the two lines
of names of variables and handles each one of them separately. The upper
row gets filled with previous variable names for NAs because of merged
cells in the original table. The lower row gets blank string for NAs.
When concatenating, if there is a second argument for the variable, the
variable name gets padded with blank space between its two arguments.
Finally, these column names are added to the tibble. The tibble is read
again to ensure good guessing of column types.

``` r
read_muni_new <- function(path, is_online = FALSE) {
  if (is_online)
    GET(path, write_disk(path <- tempfile(fileext = get_file_ext(path))))
  
  col_names_merged <- read_excel(path, sheet = 2, skip = 3, n_max = 2, col_names = FALSE) |> 
    merge_names(names_num = 2, fill_missing = c(TRUE, FALSE)) |> 
    names()
  
  read_excel(path, sheet = 2, skip = 5, col_names = col_names_merged)
}

df_2018 <- read_muni_new("data/municipalities/2018.xlsx")
```

## Import a single variable from a CBS municipalities file (2016 and later) with a single variable

``` r
read_muni_new_var <- function(path, var_name, col_num, is_online = FALSE) {
  if(is_online)
    GET(path, write_disk(path <- tempfile(fileext = get_file_ext(path))))  
  
  read_excel(path, sheet = 2, skip = 5, col_names = FALSE) |> 
  select(
    muni_id = 2,
    "{var_name}" := all_of(col_num)
  )
}
```

## Import a single variable from a CBS municipalities file (2015 and before) with a single variable

This function is important because some SELA data is using population
data older than 2018. This function takes the url of the file, the
wanted column numbers for the cities and for the regional councils. It
returns a tibble with a municipality id and the wanted variable values.

``` r
read_muni_old_var <- function(path, var_name, col_num_1, col_num_2, skip_rows = 1, is_online = FALSE) {
  if(is_online)
    GET(path, write_disk(path <- tempfile(fileext = get_file_ext(path))))  
  
  df1 <- read_excel(path, sheet = 2, skip = skip_rows)
  df2 <- read_excel(path, sheet = 4, skip = skip_rows)
  
  df1 <- df1 |> 
    select(
      muni_id = 2,
      "{var_name}" := all_of(col_num_1)
    ) |> 
    filter(str_length(muni_id) == 4)
  
  df2 <- df2 |> 
    select(
      muni_id = 2,
      "{var_name}" := all_of(col_num_2)
    ) |> 
    filter(str_length(muni_id) == 2)
  
  bind_rows(df1, df2) 
}
```

## Municipality id

Every municipality has different ids for different authorities. This
function reads the requested ids and includes their names if requested.

``` r
read_muni_id <- function(id_types = c("cbs", "edu", "tax"), include_names = FALSE) {

  data <- read_csv("https://raw.githubusercontent.com/matanhakim/general_files/main/muni_ids.csv", col_types = cols(.default = "c"))
    
  if (include_names)
  {
    data |> 
      select(contains(id_types))
  }
  else
  {
    data |> 
      select(contains(id_types) & ends_with("id"))
  }
}
```

## Yishuvim

This function reads a specific variable from the yishuvim data,
alongside its yishuv id.

``` r
read_yishuv <- function(var_name, col_num) {
  read_excel("data/yishuvim/bycode2021.xlsx", col_types = "text") |> 
    select(
      yishuv_id = 2,
      "{var_name}" := all_of(col_num)
    ) |> 
    mutate(
      yishuv_id = fix_yishuv_id(yishuv_id)
    )
}
```

This is a specific function that matches yishuv and municipality id.

``` r
match_yishuv_muni <- function() {
  
  read_yishuv("muni_id", 9) |> 
    mutate(
      muni_id = case_when(
        (muni_id == "0" | muni_id == "99") ~ yishuv_id,
        TRUE ~ str_pad(muni_id, width = 2, side = "left", pad = "0")
      )
    )
}
```

This function reads all possible names for yishuvim and their CBS yishuv
id.

``` r
read_yishuv_names <- function() {
  read_csv(
    "https://raw.githubusercontent.com/matanhakim/general_files/main/yishuv_names.csv",
    col_types = cols(.default = "c")
  )
}
```

## 2013 CBS SES data

This function reads the 2013 CBS SES data for municipalities that is
being used by 2018 SELA regulations to determine eligibility of
municipalities. It reads the file, selects the relevant variables,
removes excess rows, and transforms the id to the usual format.

``` r
read_ses_2013 <- function(url) {
  
  read_excel("data/municipalities/t02.xls", skip = 6) |> 
    slice(2:256) |>
    select(
      muni_status = 1,
      muni_id = 2,
      ses_2013_i = 5,
      ses_2013_r = 6,
      ses_2013_c = 7
    ) |> 
    mutate(
      muni_id = as.character(muni_id),
      muni_id = case_when(
        (muni_status == "0" | muni_status == "99") ~ str_pad(muni_id, width = 4, side = "left", pad = "0"),
        TRUE ~ str_pad(muni_status, width = 2, side = "left", pad = "0")
      ),
      across(c(ses_2013_r, ses_2013_c), parse_double)
    ) |> 
    select(!muni_status)
}
```

## 2004 CBS periphery data

Important to note that this is and old indicator, therefore since then
some municipal jurisdiction changes have happened:

``` r
read_peri_2004 <- function() {
  df <- read_excel("data/indices/24_08_160t2.xls", skip = 7) 
  
  df |> 
    select(
      muni_id = 1,
      peri_2004_i = 9,
      peri_2004_r = 10,
      peri_2004_c = 11
    ) |> 
    mutate(
      muni_id = as.character(muni_id),
      muni_id = case_when(
        str_length(muni_id) == 5 ~ str_sub(muni_id, start = -2),
        TRUE ~ fix_yishuv_id(muni_id)
      )
    )
}
```

# Elections data

This function reads the raw 2015 elections data file and adds a
municipality id for every yishuv.

``` r
read_elect_muni <- function() {
  read_excel("data/elections/results_20.xls") |> 
    rename(yishuv_id = 2) |> 
    mutate(yishuv_id = fix_yishuv_id(yishuv_id)) |> 
    left_join(match_yishuv_muni(), join_by(yishuv_id))
}
```

This function computes voting percentages for Likud and coalition
parties in every municipality. It filters out NA values (like Beduin
tribes). In 2015, there were no voting in Ein Kinya.

``` r
read_elect_pct <- function() {
  
  read_elect_muni() |>  
    rename(
      pot_votes = 4,
      elec_good_votes = 7,
      yahadut_hatorah = 9,
      habait_hayehudi = 14,
      kulanu = 19,
      israel_beytenu = 20,
      halikud = 21,
      shas = 33
    ) |> 
    mutate(
      coal = yahadut_hatorah +
        habait_hayehudi +
        kulanu +
        israel_beytenu +
        halikud +
        shas
    ) |> 
    group_by(muni_id) |> 
    summarize(
      elec_likud_votes = sum(halikud),
      elec_coal_votes = sum(coal),
      elec_likud_pct = 100 * sum(halikud) / sum(elec_good_votes),
      elec_coal_pct = 100 * sum(coal) / sum(elec_good_votes),
      elec_pot_votes = sum(pot_votes),
      elec_good_votes = sum(elec_good_votes)
    ) |> 
    filter(!is.na(muni_id))
}
```

# Organizations data

## Amutot

This function reads every registered amuta from guidestar and returns
its organiztion (tax) id and the name of its registered yishuv

``` r
read_amutot <- function() {
  
  df_amutot_new <- read_excel("data/organizations/דוח חודשי גיידסטאר.xlsx", sheet = 2) |> 
    select(
      tax_id = 2,
      yishuv_name = 14
    ) |> 
    mutate(
      tax_id = as.character(tax_id),
      yishuv_name = clean_yishuv_name(yishuv_name)
    )
  
  df_amutot_old <- read_excel("data/organizations/דוח גיידסטאר - אוגוסט 2020.xlsx") |> 
    select(
      tax_id = 2,
      yishuv_name = 28
    ) |> 
    mutate(
      tax_id = as.character(tax_id),
      yishuv_name = clean_yishuv_name(yishuv_name)
    )
  
  bind_rows(
    df_amutot_new,
    df_amutot_old
  ) |> 
    arrange(tax_id, yishuv_name) |> 
    distinct(tax_id, .keep_all = TRUE)
}
```

## Companies

``` r
read_companies <- function() {
  read_csv("data/organizations/companies.csv") |> 
    select(
      tax_id = 1,
      yishuv_name = 13
    ) |> 
    filter(!is.na(yishuv_name)) |> 
    mutate(
      tax_id = as.character(tax_id),
      yishuv_name = clean_yishuv_name(yishuv_name)
    )
}
```

## Municipalities

``` r
read_muni_names <- function() {
  read_muni_id(include_names = TRUE) |> 
    select(!c(edu_id, cbs_id)) |> 
    pivot_longer(!tax_id, names_to = "var", values_to = "yishuv_name") |> 
    select(!var) |> 
    distinct(yishuv_name, .keep_all = TRUE)
}
```

## Organiztions with no record

``` r
read_organizations_bad_names <- function() {
  tibble(
    tax_id = c(
      "589931187", # אוניברסיטת תל אביב
      "500701628", # אוניברסיטת חיפה
      "580303808", # תזמורת יד חריף (צרעה)
      "580503605", # תזמורת נתיה הקאמרית הקיבוצית
      "510101819", # חברת נכון בע"מ (חיפה) 
      "589120880", # המרכז לאמנות יהודית רוסית (תל אביב)
      "511854788", # מתנס רמת נגב
      "512103383", # תאטרון ענתות (ראשון לציון)
      "510021298", # סול בעמ ( לא ידוע)
      "501501183", # ועדה מוניציפאלית חברון
      "510318652", # מקיצי נרדמים בעמ ( תל אביב)
      "510550767", # לאובק חיפה
      "500217229", # מגדל תפן
      "580409449", # עמותת יוצאי טורקיה בישראל (יהוד-מונוסון)
      "510356777", # המכון לתרגום ספרות עברית (תל אביב)
      "580270858", # פורום עולים ידידות (חולון)
      "510497464", # התאטרון הפתוח בעמ (תל אביב)
      "580374270", # אמנות המשחק לתיאטרון ולקולנוע
      "580070845", # מרכז זלמן שזר (ירושלים)
      "580510097", # להקת המחול מוריה קונג (תל אביב)
      "580520229", # אקס טריטוריה (תל אביב)
      "580114965", # כת עת אל-שרק (שפרעם)
      "580373777", # אנסמבל תיאטרון הרצליה
      "500500962", # יד יצחק בן-צבי (ירושלים)
      "580302909" # תאיר - מרכז לתרבות יהודית (תל אביב)
    ),
    yishuv_name = c(
      "תל אביב - יפו",
      "חיפה",
      "צרעה",
      "נתניה",
      "חיפה",
      "תל אביב - יפו",
      "רמת נגב",
      "ראשון לציון",
      NA,
      "חברון",
      "תל אביב - יפו",
      "חיפה",
      "מגדל תפן",
      "יהוד-מונוסון",
      "תל אביב - יפו",
      "חולון",
      "תל אביב - יפו",
      "תל אביב - יפו",
      "ירושלים",
      "תל אביב - יפו",
      "תל אביב - יפו",
      "שפרעם",
      "הרצליה",
      "ירושלים",
      "תל אביב - יפו"
    )
  )
}
```

## Match yishuv id to organizations

``` r
read_organizations <- function() {
  bind_rows(
    read_amutot(),
    read_companies(),
    read_muni_names(),
    read_organizations_bad_names()
  ) |> 
    arrange(tax_id, yishuv_name) |> 
    distinct(tax_id, .keep_all = TRUE) |> 
    left_join(read_yishuv_names(), join_by(yishuv_name))
}
```

# Budget data

## Sela

This function reads SELA budget by the ministry of culture from the
years 2016-2019.

``` r
read_sela_budget <- function() {
  
  read_excel("data/budget/תמיכות המשרד לגופי תרבות 2016-2019.xlsx", sheet = 39) |> 
    slice(-1) |> 
    select(
      tax_id = 1,
      # tax_name = 2,
      init_2016 = 3,
      fest_2016 = 4,
      sela_2016 = 5,
      init_2017 = 7,
      fest_2017 = 8,
      sela_2017 = 9,
      init_2018 = 11,
      fest_2018 = 12,
      sela_2018 = 13,
      init_2019 = 15,
      fest_2019 = 16,
      sela_2019 = 17
    ) |> 
    pivot_longer(!tax_id, names_to = c("budget_type", "year"), names_sep = "_", values_to = "budget") |> 
    replace_na(list(budget = 0)) |> 
    mutate(
      year = as.numeric(year)
      ) |> 
    pivot_wider(names_from = "budget_type", names_prefix = "budget_approved_", values_from = "budget") |> 
    left_join(read_muni_id(c("tax", "cbs")), join_by(tax_id)) |> 
    select(
      muni_id = cbs_id,
      !tax_id
    )
}
```

## Culture (total)

This function reads the raw data from the Open Budget website of the
ministry of culture, and summarizes it by year and organization.

``` r
read_culture_budget <- function() {
  read_csv("data/budget/מינהל התרבות__ פירוט כל התמיכות מתקציב זה שאושרו ב כל השנים.csv") |> 
    select(
      tax_id = 6,
      year = 7,
      budget_approved_culture = 8,
      budget_paid_culture = 9
    ) |> 
    replace_na(
      list(budget_approved_culture = 0, budget_paid_culture = 0)
    ) |> 
    mutate(tax_id = as.character(tax_id)) |> 
    summarise(
      .by = c(tax_id, year),
      budget_approved_culture = sum(budget_approved_culture),
      budget_paid_culture = sum(budget_paid_culture)
    )
}
```

This chunk checks which organizations that got budget from the ministry
of culture do not appear in the current organizations data.

``` r
df_culture <- read_culture_budget() |> 
  left_join(read_organizations(), join_by(tax_id))

df_bad_names <- df_culture |> 
  filter(is.na(yishuv_id)) |> 
  distinct(yishuv_name, .keep_all = TRUE)
```

This function matches every organization supported by the ministry of
culture with its municipality id. this leaves us with a municipality id
for every budget support of the ministry of culture for every
organization in every year. missing values include yishuvim not part of
any municipality, like Mikveh Israel and the airport, and budgets that
do not go to organiztions, mostly prizes for individuals.

``` r
add_culture_budget_muni_id <- function() {
  df_culture <- read_culture_budget() |> 
    left_join(read_organizations(), join_by(tax_id)) |> 
    left_join(match_yishuv_muni(), join_by(yishuv_id)) |> 
    mutate(
      muni_id = case_when(
        !is.na(muni_id) ~ muni_id,
        str_length(yishuv_id) == 2 ~ yishuv_id
      )
    )
}
```

This function summarizes the cultural budget data by municipality and
year.

``` r
culture_budget_by_muni <- function() {
  add_culture_budget_muni_id() |> 
    summarise(
      .by = c(year, muni_id),
      budget_approved_culture = sum(budget_approved_culture),
      budget_paid_culture = sum(budget_paid_culture)
    )
}
```

# CBS cluster data

## National priority settlements decided by the Israeli government

Since the SELA budget relies also on national priority areas, these data
are needed to be imported.

### Getting the list of tables from the national priority webpage

This function reads the table data in the national priority areas
government decision webpage, and returns a list of those tables.

``` r
get_nat_pri_list <- function(){
  # nat_pri_url <- "https://www.gov.il/he/departments/policies/2013_des667"
  # 
  # read_html(nat_pri_url) |>
  #   html_elements("table") |> 
  #   html_table()
  
  list(
    read_csv("data/national_priority/nafot_priority.csv", col_names = FALSE),
    read_csv("data/national_priority/yishuvim_border.csv"),
    read_csv("data/national_priority/yishuvim_priority.csv")
  )
}
```

### Merging national priority yishuvim, subdistricts (Nafot) and yishuvim close to the border

This function reads the tables from the previous section and manipulates
them: - The nafot (subdistricts) data is added with the corresponding
nafa_id column, and converts Hebrew data to logical. - The yishuvim
declared as national priority are cleaned, added with a TRUE column and
formats the yishuv_id. - The yishuvim declared as close to the border or
threatened are cleaned, added with a TRUE column and formats the
yishuv_id. - The whole yishuvim list is being called from the CBS
website, and then all other three data frames are joined. NA’s are
replaced with FALSE, and a final national priority variable for each
yishuv is calculated. - Finally, yishuvim with NA as municipality are
filtered out, and a final national priority variable for each
municipality is calculated as having either more than 75% of yishuvim in
the municipality as national priority, or more than 50% of yishuvim in
the municipality as close to the border or threatened.

``` r
read_nat_pri_munis <- function(){
  
  pri_list <- get_nat_pri_list()
  
  pri_nafot <- pri_list[[1]] |> 
    add_column(nafa_id = c(29,21,24,62,22,23,71,32,11,61,31,41,44,43,42,51)) |> 
    mutate(
      nafa_id = as.character(nafa_id),
      nafa_nat_pri = (X7 == "כן")
    ) |> 
    select(nafa_id, nafa_nat_pri)
  
  pri_yishuvim <- pri_list[[2]] |> 
    select(yishuv_id = 1) |> 
    slice_tail(n = -1) |> 
    add_column(yishuv_nat_pri = TRUE) |> 
    mutate(yishuv_id = fix_yishuv_id(yishuv_id))
  
  pri_border <- pri_list[[3]] |> 
    select(yishuv_id = 1) |> 
    slice_tail(n = -1) |> 
    add_column(border_nat_pri = TRUE) |> 
    mutate(yishuv_id = fix_yishuv_id(yishuv_id))
  
  pri_df <- match_yishuv_muni() |> 
    left_join(read_yishuv("nafa_id", 6), join_by(yishuv_id)) |> 
    left_join(pri_nafot, by = "nafa_id") |> 
    left_join(pri_yishuvim, by = "yishuv_id") |> 
    left_join(pri_border, by = "yishuv_id") |> 
    mutate(
      across(ends_with("pri"), \(x) replace_na(x, FALSE)),
      is_nat_pri = nafa_nat_pri | yishuv_nat_pri | border_nat_pri
    )
    
  pri_df |> 
  filter(!is.na(muni_id)) |> 
  group_by(muni_id) |> 
  summarise(is_nat_pri = (mean(is_nat_pri) > 0.75) | (mean(border_nat_pri) >= 0.5))
    
}
```

# Create a complete data frame

## Read population for every year

``` r
df_pop_args_old <- tribble(
  ~path, ~var_name, ~col_num_1, ~col_num_2, ~skip_rows, ~year,
  "",    "pop",     15,          30,         0,         2013,
  "",    "pop",     16,          33,         1,         2014,
  "",    "pop",     14,          31,         1,         2015
) |> 
  mutate(
    path = str_c("data/municipalities/", year, ".xls")
  ) |> 
  select(!year)

df_pop_args_new <- tibble(
      path = str_c("data/municipalities/", 2016:2019, ".xlsx"),
      var_name = "pop",
      col_num = 13
    )
df_pop <- bind_rows(
  pmap(df_pop_args_old, read_muni_old_var),
  pmap(df_pop_args_new, read_muni_new_var)
) |> 
  mutate(
    year = rep(2013:2019, each = 255),
    pop = if_else(pop < 1000, pop * 1000, pop)
  )
```

## Classify municipalities to sector (Jewish/Arab)

``` r
df_sector <- read_muni_new_var("data/municipalities/2019.xlsx", "arab_pct", 16) |> 
  mutate(
    arab_pct = as.numeric(str_replace(arab_pct, "-", "0")),
    sector = as_factor(if_else(arab_pct > 50, "arab", "jewish"))
  ) |> 
  select(!arab_pct)
```

## Read periphery 2015 indices

``` r
df_peri_2015 <- read_muni_new("data/municipalities/2016.xlsx") |> 
  select(
    muni_id = 2,
    muni_type = 4,
    peri_2015_c = 187,
    peri_2015_i = 188,
    peri_2015_r = 189
  )
```

## Combine all data frames

``` r
df <- expand_grid(
  year = 2013:2019,
  read_muni_id(id_types = "cbs", include_names = TRUE)
) |> 
  rename(
    muni_id = cbs_id,
    muni_name = cbs_name
  ) |> 
  left_join(culture_budget_by_muni(), join_by(year, muni_id)) |> 
  left_join(read_sela_budget(), join_by(year, muni_id)) |> 
  left_join(df_pop, join_by(year, muni_id)) |> 
  left_join(df_sector, join_by(muni_id)) |> 
  left_join(read_ses_2013(), join_by(muni_id)) |> 
  left_join(read_peri_2004(), join_by(muni_id)) |> 
  left_join(df_peri_2015, join_by(muni_id)) |> 
  left_join(read_nat_pri_munis(), join_by(muni_id)) |> 
  left_join(read_elect_pct(), join_by(muni_id))
```

## Recode and add variables

``` r
na_peri_muni <- c(
  "0483", # Bueina
  "0628", # Jat
  "0490", # Dir ElAssad
  "0534", # Osfia
  "69", # AlQasum
  "68" # Neve Midbar
)

df <- df |> 
  mutate(
    across(contains(c("budget", "elec")), \(x) replace_na(x, 0)),
    budget_approved_culture_per_capita = budget_approved_culture / pop,
    peri_2004_i = replace_match(peri_2004_i, muni_id, na_peri_muni, peri_2015_i),
    peri_2004_r = replace_match(peri_2004_r, muni_id, na_peri_muni, peri_2015_r),
    peri_2004_c = replace_match(peri_2004_c, muni_id, na_peri_muni, peri_2015_c)
  )
```

# Inequality measures

## Top 10%

``` r
top_prop <- function(var, weights = 1, top_prop = 0.1) {
  tibble(x = var, w = weights) |> 
    uncount(w) |> 
    arrange(desc(x)) |> 
    slice_head(prop = top_prop) |> 
    summarise(top10_pct = sum(x) / sum(var * weights)) |> 
    pull(top10_pct)
}
```

## Gini

``` r
gini_weighted <- function(var, weights) {
  tibble(x = var, w = weights) |> 
    uncount(w) |> 
    summarise(gini = reldist::gini(x)) |> 
    pull(gini)
}
```

## Total budget

``` r
df |> 
  summarise(
    .by = year,
    budget_tot = sum(budget_approved_culture),
    budget_per_capita = sum(budget_approved_culture) / sum(pop)
  ) |> 
  pivot_longer(contains("budget"), names_to = "var", values_to = "value") |> 
  ggplot(aes(year, value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ var, scales = "free_y", labeller = labeller(var = c(budget_per_capita = "תקציב מינהל תרבות לתושב", budget_tot = "תקציב מינהל תרבות כולל"))) +
  scale_y_continuous(
    labels = label_comma(),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    x = "שנה",
    y = 'ש"ח'
  )
```

## Inequality calculation

``` r
df_gini <- df |> 
  summarise(
    .by = year,
    # gini_1 = acid::weighted.gini(budget_approved_culture_per_capita, w = pop)[[1]],
    # gini_2 = dineq::gini.wtd(budget_approved_culture_per_capita, weights = pop),
    gini = gini_weighted(budget_approved_culture_per_capita, weights = as.integer(pop)),
    # top10_pct_1 = 1 - DescTools::Quantile(budget_approved_culture, weights = pop, probs = 0.9) / sum(budget_approved_culture * pop),
    # top10_pct_2 = 1 - reldist::wtd.quantile(budget_approved_culture, weight = pop, q = 0.9) / sum(budget_approved_culture * pop),
    top10_pct = top_prop(budget_approved_culture_per_capita, weights = as.integer(pop), top_prop = 0.1),
    bot50_pct = 1 - top_prop(budget_approved_culture_per_capita, weights = as.integer(pop), top_prop = 0.5),
    mid50_90_pct = 1 - top10_pct - bot50_pct
  )
```

``` r
df_ineq_sector <- df |> 
  summarise(
    .by = c(year, sector),
    budget_per_capita = sum(budget_approved_culture) / sum(pop)
  )

df_ineq_ses_c <- df |> 
  mutate(
    ses_2013_c = fct_collapse(
      factor(ses_2013_c),
      `אשכולות 1-3` = c("1", "2", "3"),
      `אשכולות 4-6` = c("4", "5", "6"),
      `אשכולות 7-10` = c("7", "8", "9", "10")
    )
  ) |> 
  summarise(
    .by = c(year, ses_2013_c),
    budget_per_capita = sum(budget_approved_culture) / sum(pop)
  )

df_ineq_ses <- df |> 
  summarise(
    .by = c(year, ses_2013_c),
    budget_per_capita = sum(budget_approved_culture) / sum(pop)
  )

df_ineq_peri <- df |> 
  summarise(
    .by = c(year, peri_2015_c),
    budget_per_capita = sum(budget_approved_culture) / sum(pop)
  )

df_ineq_peri_c <- df |> 
  mutate(
    peri_2015_c = fct_collapse(
      factor(peri_2015_c),
      `אשכולות 1-3` = c("1", "2", "3"),
      `אשכולות 4-6` = c("4", "5", "6"),
      `אשכולות 7-10` = c("7", "8", "9", "10")
    )
  ) |> 
  summarise(
    .by = c(year, peri_2015_c),
    budget_per_capita = sum(budget_approved_culture) / sum(pop)
  )

df_clusters <- df |> 
  pivot_longer(c(ses_2013_c, peri_2015_c), names_to = "cluster_type", values_to = "cluster_value") |> 
  mutate(
    cluster_value = fct_collapse(
      factor(cluster_value),
      `אשכולות 1-3` = c("1", "2", "3"),
      `אשכולות 4-6` = c("4", "5", "6"),
      `אשכולות 7-10` = c("7", "8", "9", "10")
    )
  ) |> 
  summarise(
    .by = c(year, cluster_type, cluster_value),
    budget_per_capita = sum(budget_approved_culture) / sum(pop)
  )

df_ineq_type <- df |> 
  summarise(
    .by = c(year, muni_type),
    budget_per_capita = sum(budget_approved_culture) / sum(pop)
  )
```

## Visualize

``` r
df_gini |> 
  pivot_longer(!year, names_to = "statistic", values_to = "value") |> 
    filter(statistic == "gini") |> 
  ggplot(aes(year, value)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(
    data = df_gini |> 
      pivot_longer(!year, names_to = "statistic", values_to = "value") |> 
      filter(statistic == "gini", year %in% c(2014,2019)),
    aes(label = round(value, 3)),
    vjust = -1
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.1, 0.1))
  ) +
  scale_x_continuous(breaks = 2013:2019) +
  labs(
    x = "שנה",
    y = "ערך מדד ג'יני"
  )
```

``` r
df_gini |> 
  pivot_longer(!year, names_to = "statistic", values_to = "value") |> 
  filter(statistic != "gini") |> 
  ggplot(aes(year, value, color = statistic, shape = statistic)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(
    data = df_gini |> 
      pivot_longer(!year, names_to = "statistic", values_to = "value") |> 
      filter(statistic != "gini", year %in% c(2014,2019)),
    aes(label = percent(value, 0.1)),
    vjust = -1,
    show.legend = FALSE
  ) +
  scale_y_continuous(
    labels = label_percent(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_continuous(breaks = 2013:2019) +
  scale_color_discrete(labels = decile_labs) +
  scale_shape_discrete(labels = decile_labs) +
  guides(
    color = guide_legend(reverse = TRUE),
    shape = guide_legend(reverse = TRUE)
  ) +
  theme(
    legend.title= element_blank(),
    legend.position = c(0.25, 0.3),
    legend.background = element_rect(fill = "white", color = "black")
  ) +
    labs(
    x = "שנה",
    y = "חלקה של כל קבוצה מתוך תקציב מינהל תרבות"
  )
```

``` r
df_ineq_sector |> 
  plot_line_group(sector, legend_labs = sector_labs) +
  theme(
    legend.position = c(0.25, 0.4)
  ) +
  labs(
    x = "שנה",
    y = 'תקציב מינהל תרבות לתושב (ש"ח)'
  )
```

``` r
df_ineq_type |> 
  plot_line_group(muni_type) +
  theme(
    legend.position = c(0.25, 0.35)
  ) +
  labs(
    x = "שנה",
    y = 'תקציב מינהל תרבות לתושב (ש"ח)'
  )
```

``` r
df_ineq_ses_c |> 
  mutate(ses_2013_c = fct_reorder2(ses_2013_c, year, budget_per_capita)) |> 
  plot_line_group(ses_2013_c) +
  theme(
    legend.position = c(0.8, 0.2)
  ) +
  labs(
    x = "שנה",
    y = 'תקציב מינהל תרבות לתושב (ש"ח)'
  )
```

``` r
df_ineq_peri_c |> 
  plot_line_group(peri_2015_c) +
  theme(
    legend.position = c(0.8, 0.2)
  ) +
  labs(
    x = "שנה",
    y = 'תקציב מינהל תרבות לתושב (ש"ח)'
  )
```

``` r
df_clusters |> 
  plot_line_group(cluster_value) +
  facet_wrap(
    ~ cluster_type,
    labeller = labeller(cluster_type = c(
      peri_2015_c = str_rtl("אשכול פריפריאליות (2015)"),
      ses_2013_c = str_rtl("אשכול חברתי-כלכלי (2013)"))
    )
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  theme(
    legend.position = c(0.9, 0.15)
  ) +
  labs(
    x = "שנה",
    y = 'תקציב מינהל תרבות לתושב (ש"ח)'
  )
```

# Growth rate of budget by group

``` r
df_ineq_sector |> 
  summarise(
    .by = sector,
    growth_rate = (last(budget_per_capita) - first(budget_per_capita)) / first(budget_per_capita)
  )

df_ineq_ses |> 
  summarise(
    .by = ses_2013_c,
    growth_rate = (last(budget_per_capita) - first(budget_per_capita)) / first(budget_per_capita)
  ) |> 
  arrange(growth_rate)

df_ineq_ses |> 
  summarise(
    .by = ses_2013_c,
    added_budget_per_capita = (last(budget_per_capita) - first(budget_per_capita))
  ) |> 
  ggplot(aes(ses_2013_c, added_budget_per_capita)) +
  geom_point()
```

``` r
df_ineq_peri |> 
  summarise(
    .by = peri_2015_c,
    growth_rate = (last(budget_per_capita) - first(budget_per_capita)) / first(budget_per_capita)
  ) |> 
  arrange(growth_rate)

df_ineq_peri |> 
  summarise(
    .by = peri_2015_c,
    added_budget_per_capita = (last(budget_per_capita) - first(budget_per_capita))
  ) |> 
  ggplot(aes(peri_2015_c, added_budget_per_capita)) +
  geom_point()
```

``` r
df |> 
  summarise(
    .by = muni_name,
    added_budget_per_capita = nth(budget_approved_culture_per_capita, -2) - nth(budget_approved_culture_per_capita, 2),
    ses_2013_i = mean(ses_2013_i)
  ) |> 
  arrange(added_budget_per_capita) |> 
  ggplot(aes(ses_2013_i, added_budget_per_capita)) +
  geom_point() +
  geom_smooth(se = FALSE)
```

# Sela Analysis

## Sela 2018 eligibility

``` r
df_2018 <- df |> 
  filter(year == 2018) |> 
  left_join(df |> filter(year == 2015) |> select(muni_id, pop_2015 = pop), join_by(muni_id))
```

### Festival eligibility

The population year determined to use to decide eligibility is 2015.
using 2018 created some municipalities that changed population category
between the years. the 2015 year was the latest available during the
start of 2018.

``` r
df_2018 <- df_2018 |> 
  mutate(
    is_elig_fest = case_when(
      pop_2015 > 100000 ~ FALSE,
      ses_2013_c >= 6 + 2 ~ FALSE,
      ses_2013_c <= 6 ~ TRUE,
      muni_type == "מועצה אזורית" & peri_2004_c <= 2 ~ TRUE,
      is_nat_pri ~ TRUE,
      TRUE ~ FALSE
    ),
    budget_elig_fest = case_when(
      !is_elig_fest ~ 0,
      pop_2015 <= 5000 ~ 70000,
      pop_2015 <= 20000 ~ 115000,
      pop_2015 > 20000 ~ 200000
    )
  )
```

### Initiatives eligibility

The eligibility score of each municipality is calculated: first the
score is calculated according the the regulations. the score is
normalized between all eligible municipalities and then multiplied by a
number close to the total budget allocated. the specific number was
decided through trial and error to match the vast majority of
municipalities.

``` r
df_2018 <- df_2018 |> 
  mutate(
    score_elig_init = case_when(
      pop_2015 <= 10000 ~ 1,
      pop_2015 <= 50000 ~ 2,
      pop_2015 <= 100000 ~ 3,
      pop_2015 <= 150000 ~ 4,
      pop_2015 <= 200000 ~ 5,
      pop_2015 <= 500000 ~ 6,
      pop_2015 > 500000 ~ 7
    ),
    score_elig_init = case_when(
      pop_2015 > 100000 ~ score_elig_init,
      is_nat_pri ~ score_elig_init * 2,
      ses_2013_c <= 6 ~ score_elig_init * 2,
      peri_2004_c <= 2 ~ score_elig_init * 2,
      TRUE ~ score_elig_init
    ),
    budget_elig_init = 23907075 * score_elig_init / sum(score_elig_init * (budget_approved_init > 0)),
    budget_elig_tot = budget_elig_fest + budget_elig_init
  )
```

## Modelling Sela budget

### Check distribution of dependant variable

``` r
df_2018 |> 
  ggplot(aes(budget_elig_tot)) +
  geom_histogram() +
  theme_minimal() +
  scale_x_continuous(labels = label_comma()) +
  labs(
    x = 'גובה הזכאות של רשות מקומית לתמיכה תקציבית של תקנת סל"ע בשנת 2018 (ש"ח)',
    y = "מספר רשויות"
  )
```

``` r
df_2018 |> 
  summarise(
    mean = mean(budget_elig_tot),
    sd = sd(budget_elig_tot),
    skewness = skewness(budget_elig_tot),
    kurtosis = kurtosis(budget_elig_tot)
  )
```

### Distribution table of all variables

``` r
df_2018 |> 
  select(
    budget_elig_tot,
    pop_2015,
    ses_2013_c,
    peri_2004_c,
    sector,
    elec_likud_pct
  ) |> 
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})")
  )
```

### Model with Likud voting percent

``` r
sela_mdl2 <- lm(budget_elig_tot ~ sector * elec_likud_pct, data = df_2018)

sela_mdl2 %>% 
  tidy()

sela_mdl2 %>% 
  glance()

sela_mdl2 %>% 
  augment() %>% 
  ggplot(aes(elec_likud_pct, budget_elig_tot, color = sector)) + 
  geom_line(aes(y = .fitted)) +
  geom_point()
```

This shows a good relationship so that jewish municipalities are
receiveing more budget as the Likud voting percent rises.

``` r
sela_mdl3 <- lm(budget_elig_tot ~ sector : elec_likud_pct + pop_2015 + ses_2013_c + peri_2004_c, data = df_2018)
sela_mdl3_control <- lm(budget_elig_tot ~ pop_2015 + ses_2013_c + peri_2004_c, data = df_2018)
sela_mdl3_control2 <- lm(budget_elig_tot ~ pop_2015 + ses_2013_c + peri_2004_c + is_nat_pri, data = df_2018)
anova(sela_mdl3_control, sela_mdl3)

sela_mdl3 %>% 
  tidy()

sela_mdl3 %>% 
  glance()

sela_mdl3_control %>% 
  tidy()

sela_mdl3_control %>% 
  glance()

sela_mdl3 %>% 
  augment() %>% 
  ggplot(aes(elec_likud_pct, budget_elig_tot, color = sector)) + 
  geom_line(aes(y = .fitted)) +
  geom_point()
```

``` r
sela_mdl3 %>% 
  augment() %>% 
  ggplot(aes(elec_likud_pct, .fitted - sela_mdl3_control %>% augment() %>% pull(.fitted), color = sector)) + 
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
```

The model is still significant for Likud voting percentage after adding
control variables.

Let’s create a better plot for the last plot of the difference between
the two models.

``` r
sela_mdl3 %>% 
  augment() %>% 
  ggplot(aes(
    elec_likud_pct, .fitted - sela_mdl3_control %>% augment() %>% pull(.fitted),
    color = sector,
    shape = sector
  )) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = label_comma()) +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  scale_color_discrete(labels = c("ערבי", "יהודי")) +
  scale_shape_discrete(labels = c("ערבי", "יהודי")) +
  guides(shape = guide_legend(override.aes = list(size = 3))) +
  theme(legend.position = "bottom") +
  labs(
    x = "אחוז הצבעה לליכוד בבחירות 2015",
    y = '\u202B"דיבידנד הנאמנות בתרבות" (ש"ח)',
    color = "מגזר",
    shape = "מגזר"
  )
```

Let’s put the two models in a table.

``` r
tbl_merge(
  list(
    tbl_regression(
      sela_mdl3_control,
      intercept = TRUE,
      conf.int = FALSE
    ) %>% 
      modify_header(
        statistic = "**t-statistic**",
        std.error = "**SE**"
      ) %>% 
      bold_p() %>% 
      add_glance_table(
        include = c(r.squared, statistic)
      )
    ,
    tbl_regression(
      sela_mdl3,
      intercept = TRUE,
      conf.int = FALSE
    ) %>% 
      modify_header(
        statistic = "**t-statistic**",
        std.error = "**SE**"
      ) %>% 
      bold_p() %>% 
      add_glance_table(
        include = c(r.squared, statistic)
      )
  ),
  tab_spanner = c("M1", "M2")
) %>% 
  modify_table_body(~.x %>% arrange(row_type == "glance_statistic")) %>% 
  as_flex_table()
```

### Check model outcomes

Let’s check the distribution of voting to Likud across SES clusters.

``` r
elec_more_df <- df %>% 
  mutate(
    likud_votes = elec_good_votes * (elec_likud_pct / 100),
    ses_2013_c = factor(ses_2013_c)
  )

elec_more_df %>% 
  group_by(ses_2013_c) %>% 
  summarise(elec_likud_pct = round(sum(likud_votes) / sum(elec_good_votes) * 100, 1)) %>% 
  ggplot(aes(ses_2013_c, elec_likud_pct)) +
  geom_col() + 
  geom_text(
      aes(label = paste0(round(elec_likud_pct, digits = 1), "%")),
      vjust = -0.5
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    x = str_rtl("אשכול חברתי-כלכלי (2013)"),
    y = str_rtl("אחוז הצבעה לליכוד בבחירות 2015")
  )
```

Since SES cluster 7 had special criteria, Let’s look at the Likud voting
patterns within them.

``` r
ses_7_df <- elec_more_df %>% 
  filter(ses_2013_c == "7")

ses_7_df %>% 
  count(is_nat_pri, muni_type == "מועצה אזורית" & peri_2004_c <= 2)

ses_7_df %>% 
  group_by(is_nat_pri) %>% 
  summarise(elec_likud_pct = round(sum(likud_votes) / sum(elec_good_votes) * 100, 1)) %>% 
  ggplot(aes(is_nat_pri, elec_likud_pct)) +
  geom_col() + 
  geom_text(
      aes(label = paste0(round(elec_likud_pct, digits = 1), "%")),
      vjust = -0.5
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    x = "אזור עדיפות לאומית",
    y = "אחוז הצבעה לליכוד"
  )
```

``` r
# by a simple mean and not total mean
ses_7_df %>% 
  group_by(is_nat_pri) %>% 
  summarise(elec_likud_pct = round(mean(elec_likud_pct), 1)) %>% 
  ggplot(aes(is_nat_pri, elec_likud_pct)) +
  geom_col() + 
  geom_text(
      aes(label = paste0(round(elec_likud_pct, digits = 1), "%")),
      vjust = -0.5
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    x = "אזור עדיפות לאומית",
    y = "אחוז הצבעה לליכוד"
  )
```

``` r
# by eligibility criterea for initiatives
elec_more_df %>% 
  filter(ses_2013_c %in% c("7", "8", "9", "10")) %>% 
  mutate(special_elig_init = is_nat_pri | peri_2004_c <= 2) %>% 
  group_by(special_elig_init) %>% 
  summarise(elec_likud_pct = round(mean(elec_likud_pct), 1)) %>% 
  ggplot(aes(special_elig_init, elec_likud_pct)) +
  geom_col() + 
  geom_text(
      aes(label = paste0(round(elec_likud_pct, digits = 1), "%")),
      vjust = -0.5
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    x = "אזור עדיפות לאומית או רשות פריפריאלית",
    y = "אחוז הצבעה לליכוד"
  )
```

## Sensitivity analysis for Sela SES cluster threshhold

### Function for calculating eligibilty by SES cluster

``` r
calc_eligibility <- function(data, cluster) {
  data |> 
    mutate( # Festivals eligibility
      is_elig_fest = case_when(
        pop_2015 > 100000 ~ FALSE,
        ses_2013_c >= cluster + 2 ~ FALSE,
        ses_2013_c <= cluster ~ TRUE,
        muni_type == "מועצה אזורית" & peri_2004_c <= 2 ~ TRUE,
        is_nat_pri ~ TRUE,
        TRUE ~ FALSE
      ),
      budget_elig_fest = case_when(
        !is_elig_fest ~ 0,
        pop_2015 <= 5000 ~ 70000,
        pop_2015 <= 20000 ~ 115000,
        pop_2015 > 20000 ~ 200000
      )
    ) |> 
    mutate( # Initiatives eligibility
      score_elig_init = case_when(
        pop_2015 <= 10000 ~ 1,
        pop_2015 <= 50000 ~ 2,
        pop_2015 <= 100000 ~ 3,
        pop_2015 <= 150000 ~ 4,
        pop_2015 <= 200000 ~ 5,
        pop_2015 <= 500000 ~ 6,
        pop_2015 > 500000 ~ 7
      ),
      score_elig_init = case_when(
        pop_2015 > 100000 ~ score_elig_init,
        is_nat_pri ~ score_elig_init * 2,
        ses_2013_c <= cluster ~ score_elig_init * 2,
        peri_2004_c <= 2 ~ score_elig_init * 2,
        TRUE ~ score_elig_init
      ),
      budget_elig_init = 23959050 * score_elig_init / sum(score_elig_init * (budget_approved_init > 0)),
      budget_elig_tot = budget_elig_fest + budget_elig_init
    )
}
```

### Function for modeling by cluster

``` r
model_sela <- function(data) {
  sela_mdl <- lm(budget_elig_tot ~ sector : elec_likud_pct + pop_2015 + ses_2013_c + peri_2004_c, data = data)
  sela_mdl_control <- lm(budget_elig_tot ~ pop_2015 + ses_2013_c + peri_2004_c, data = data)
  mdl_anova <- anova(sela_mdl_control, sela_mdl)
  df_effect <- sela_mdl |> 
    tidy() |> 
    filter(term == "sectorjewish:elec_likud_pct")
  
  tibble(
    data = list(data),
    m1 = list(sela_mdl_control),
    m2 = list(sela_mdl),
    anova = list(mdl_anova),
    p_anova = mdl_anova[[6]][[2]],
    b_effect = df_effect$estimate,
    p_effect = df_effect$p.value,
    budget_per_likud_vote = data[[1]] |> 
      summarise(sum(budget_elig_tot * elec_likud_pct / 100) / sum(elec_likud_votes)) |>
      pull(),
    budget_per_coal_vote = data[[1]] |> 
      summarise(sum(budget_elig_tot * elec_coal_pct / 100) / sum(elec_coal_votes)) |>
      pull(),
    budget_per_left_vote = data[[1]] |> 
      summarise(sum(budget_elig_tot * (100 - elec_coal_pct) / 100) / sum(elec_good_votes - elec_coal_votes)) |>
      pull(),
    budget_per_vote = data[[1]] |> 
      summarise(sum(budget_elig_init)) |>
      pull()
  )
}
```

### Create data frame for model by SES cluster threshhold

``` r
df_sela_mdl <- map2(list(df_2018), 1:10, calc_eligibility) |> 
  map(model_sela) |> 
  list_rbind() |> 
  mutate(
    cluster = 1:10,
    p_effect_ast = case_when(
      p_effect > 0.05 ~ "",
      p_effect > 0.01 ~ "*",
      p_effect > 0.001 ~ "**",
      p_effect <= 0.001 ~ "***"
    ),
    budget_ratio = budget_per_likud_vote / budget_per_left_vote
  )

df_sela_mdl
```

``` r
df_sela_mdl |> 
  mutate(to_highlight = if_else(cluster == 6, "yes", "no")) |> 
  ggplot(aes(cluster, b_effect, label = str_c(p_effect_ast, comma(b_effect)), fill = to_highlight)) +
  geom_col() +
  geom_text(aes(vjust = -1 * sign(b_effect))) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(
    labels = label_comma(), expand = expansion(mult = c(0.1, 0.1))
  ) +
  scale_fill_manual(values = c("yes" = "darkred"), guide = "none") +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(
    x = "אשכול סף חברתי-כלכלי",
    y = 'דיבידנד נאמנות (ש"ח)',
    caption = "* p < 0.1, ** p < 0.01, *** p < 0.001"
  )
```

## Checking budget per Likud voter
