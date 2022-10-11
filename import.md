Thesis - Import Files
================

- <a href="#load-libraries" id="toc-load-libraries"><span
  class="toc-section-number">1</span> Load libraries</a>
- <a href="#municipalities-data" id="toc-municipalities-data"><span
  class="toc-section-number">2</span> Municipalities data</a>
  - <a href="#import-a-single-municipalities-file-from-cbs-2016-and-later"
    id="toc-import-a-single-municipalities-file-from-cbs-2016-and-later"><span
    class="toc-section-number">2.1</span> Import a single municipalities
    file from CBS (2016 and later)</a>
  - <a
    href="#import-a-single-municipalities-file-from-cbs-2015-and-before-with-a-single-variable"
    id="toc-import-a-single-municipalities-file-from-cbs-2015-and-before-with-a-single-variable"><span
    class="toc-section-number">2.2</span> Import a single municipalities
    file from CBS (2015 and before) with a single variable</a>
  - <a href="#getting-the-list-of-yishuvim-id-and-municipality-id"
    id="toc-getting-the-list-of-yishuvim-id-and-municipality-id"><span
    class="toc-section-number">2.3</span> Getting the list of yishuvim id
    and municipality id</a>
- <a href="#general-elections-data" id="toc-general-elections-data"><span
  class="toc-section-number">3</span> General elections data</a>
  - <a
    href="#importing-general-elections-files-from-the-elections-committee-by-url"
    id="toc-importing-general-elections-files-from-the-elections-committee-by-url"><span
    class="toc-section-number">3.1</span> Importing general elections files
    from the elections committee by url</a>
  - <a href="#adding-municipality-id-to-a-data-frame-with-yishuv_id"
    id="toc-adding-municipality-id-to-a-data-frame-with-yishuv_id"><span
    class="toc-section-number">3.2</span> Adding municipality id to a data
    frame with yishuv_id</a>
  - <a href="#manipulating-elections-data-to-fit-municipalities-data"
    id="toc-manipulating-elections-data-to-fit-municipalities-data"><span
    class="toc-section-number">3.3</span> Manipulating elections data to fit
    municipalities data</a>
- <a href="#budget-data" id="toc-budget-data"><span
  class="toc-section-number">4</span> Budget data</a>
  - <a
    href="#importing-and-manipulating-sela-budget-data-from-open-budget-for-2016-2020"
    id="toc-importing-and-manipulating-sela-budget-data-from-open-budget-for-2016-2020"><span
    class="toc-section-number">4.1</span> Importing and manipulating Sela
    budget data from Open Budget for 2016-2020</a>
  - <a
    href="#importing-and-manipulating-sela-budget-data-from-the-ministry-of-culture-for-2016-2019"
    id="toc-importing-and-manipulating-sela-budget-data-from-the-ministry-of-culture-for-2016-2019"><span
    class="toc-section-number">4.2</span> Importing and manipulating Sela
    budget data from the Ministry of Culture for 2016-2019</a>
  - <a
    href="#getting-conversion-table-between-tax-municipal-id-and-cbs-municipal-id"
    id="toc-getting-conversion-table-between-tax-municipal-id-and-cbs-municipal-id"><span
    class="toc-section-number">4.3</span> Getting conversion table between
    tax municipal id and CBS municipal id</a>
  - <a href="#adding-municipality-id-to-a-data-frame-with-budget-data"
    id="toc-adding-municipality-id-to-a-data-frame-with-budget-data"><span
    class="toc-section-number">4.4</span> Adding municipality id to a data
    frame with budget data</a>
- <a href="#sela-adiitional-data" id="toc-sela-adiitional-data"><span
  class="toc-section-number">5</span> SELA adiitional data</a>
  - <a href="#cbs-ses-data" id="toc-cbs-ses-data"><span
    class="toc-section-number">5.1</span> 2013 CBS SES data</a>
  - <a href="#cbs-periphery-data" id="toc-cbs-periphery-data"><span
    class="toc-section-number">5.2</span> 2004 CBS periphery data</a>
  - <a href="#cbs-statistical-areas-ses-data"
    id="toc-cbs-statistical-areas-ses-data"><span
    class="toc-section-number">5.3</span> 2008 CBS statistical areas SES
    data</a>
  - <a
    href="#national-priority-settlements-decided-by-the-israeli-government"
    id="toc-national-priority-settlements-decided-by-the-israeli-government"><span
    class="toc-section-number">5.4</span> National priority settlements
    decided by the Israeli government</a>
    - <a
      href="#merging-national-priority-yishuvim-subdistricts-nafot-and-yishuvim-close-to-the-border"
      id="toc-merging-national-priority-yishuvim-subdistricts-nafot-and-yishuvim-close-to-the-border"><span
      class="toc-section-number">5.4.1</span> Merging national priority
      yishuvim, subdistricts (Nafot) and yishuvim close to the border</a>
- <a href="#combining-all-data-sources-into-one-data-frame"
  id="toc-combining-all-data-sources-into-one-data-frame"><span
  class="toc-section-number">6</span> Combining all data sources into one
  data frame</a>
- <a href="#future-code-that-is-not-operatable-right-now"
  id="toc-future-code-that-is-not-operatable-right-now"><span
  class="toc-section-number">7</span> Future Code that is not operatable
  right now</a>
  - <a href="#binding-municipality-files-for-2016-2019-by-row"
    id="toc-binding-municipality-files-for-2016-2019-by-row"><span
    class="toc-section-number">7.1</span> Binding municipality files for
    2016-2019 by row</a>
  - <a href="#import-a-single-municipalities-file-2015-and-earlier"
    id="toc-import-a-single-municipalities-file-2015-and-earlier"><span
    class="toc-section-number">7.2</span> Import a single municipalities
    file (2015 and earlier)</a>

# Load libraries

``` r
library(tidyverse)
```

    Error: package or namespace load failed for 'tidyverse' in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     namespace 'rlang' 1.0.4 is already loaded, but >= 1.0.6 is required

``` r
library(readxl)
library(httr)
library(rvest)
```

    Error: package or namespace load failed for 'rvest' in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     namespace 'rlang' 1.0.4 is already loaded, but >= 1.0.6 is required

``` r
locale("he")
```

    Error in locale("he"): could not find function "locale"

# Municipalities data

## Import a single municipalities file from CBS (2016 and later)

This is a function that gets a url and returns a tibble. First, it
extracts the file extension with a regular expression, and then
downloads the file with the url parameter. later, it reads the two lines
of names of variables and handles each one of them separately. The upper
row gets filled with previous variable names for NAs because of merged
cells in the original table. The lower row gets blank string for NAs.
When concatenating, if there is a second argument for the variable, the
variable name gets padded with blank space between its two arguments.

``` r
read_muni_new <- function(url){
  file_ext <- str_extract(url, "[0-9a-z]+$")
  GET(url, write_disk(tf <- tempfile(fileext = file_ext)))
  
  df_head <- read_excel(tf, sheet = 2, skip = 3, n_max = 2, col_names = FALSE)
  
  col_names_1 <- df_head %>% 
    slice(1) %>% 
    pivot_longer(everything()) %>% 
    select(value) %>% 
    fill(value)
  
  col_names_2 <- df_head %>% 
    slice(2) %>% 
    pivot_longer(everything()) %>% 
    select(value) %>% 
    replace_na(list(value = ""))
  
  df_vars <- bind_cols(col_names_1, col_names_2) %>% 
    mutate(
      var_names = if_else(str_length(value...2) > 0,
                         str_c(value...1, " ", value...2),
                         str_c(value...1, value...2))
    )
  
  var_names <- df_vars %>% pull(var_names)
  
  df_whole <- read_excel(tf, sheet = 2, skip = 5, col_names = var_names)
  return(df_whole)

}
```

## Import a single municipalities file from CBS (2015 and before) with a single variable

This function is important because some SELA data is using population
data older than 2018. This function takes the url of the file, the
wanted column numbers for the cities and for the regional councils. It
returns a tibble with a municipality id and the wanted variable values.

``` r
read_muni_old <- function(url, col_num_1, col_num_2){
  file_ext <- str_extract(url, "[0-9a-z]+$")
  GET(url, write_disk(tf <- tempfile(fileext = file_ext)))  
  
  df1 <- read_excel(tf, sheet = 2, skip = 1)
  df2 <- read_excel(tf, sheet = 4, skip = 1)
  
  df1 <- df1 %>% 
    select(
      muni_id = 2,
      var = col_num_1
    ) %>% 
    filter(str_length(muni_id) == 4)
  
  df2 <- df2 %>% 
    select(
      muni_id = 2,
      var = col_num_2
    ) %>% 
    filter(str_length(muni_id) == 2)
  
  df <- bind_rows(df1, df2)
  df
}
```

## Getting the list of yishuvim id and municipality id

The function gets the 2021 yishuvim file from CBS, cleans it, and
returns a tibble of id’s of yishuvim by id’s of municipality. the values
are all text; regional councils have 2-numbers text id’s, and yishuvim
and other municipalities have 4-numbers text id’s. If there is no
municipality id, it means that the yishuv is either unrecognized (for
example, some Bedouin people in the Negev) or it is some sort of place
that is not under any municipality (for example, Mikveh Israel farm).

``` r
get_yishuv_muni <- function(){

  url <- "https://www.cbs.gov.il/he/publications/doclib/2019/ishuvim/bycode2021.xlsx"
  file_ext <- str_extract(url, "[0-9a-z]+$")
  GET(url, write_disk(tf <- tempfile(fileext = file_ext)))  
  
  yishuvim <- read_excel(tf, col_types = "text")
  
  yishuvim <- yishuvim %>% 
    select(
      yishuv_id = 2,
      nafa_id = 6,
      muni_id = 9
    ) %>% 
    mutate(
      yishuv_id = str_pad(yishuv_id, width = 4, side = "left", pad = "0"),
      muni_id = case_when(
        (muni_id == "0" | muni_id == "99") ~ yishuv_id,
        TRUE ~ str_pad(muni_id, width = 2, side = "left", pad = "0")
      )
    )
  
yishuvim
}
```

# General elections data

## Importing general elections files from the elections committee by url

important to note this function is currently only applicable to excel
files

``` r
read_elec_general <- function(url){
  file_ext <- str_extract(url, "[0-9a-z]+$")
  GET(url, write_disk(tf <- tempfile(fileext = file_ext)))
  
  df <- read_excel(tf)
  
  df
}
```

## Adding municipality id to a data frame with yishuv_id

The function receives a data frame and the column number of the yishuv
id as arguments. First, it calls the data frame that links between
yishuvim and municipalities. Then, it converts the yishuv_id from the
argument to a character vector, pads it with 0’s to fit the format, and
adds the municipalities id’s by the yishuv id. The function returns the
original data frame with two additional columns: the formatted yishuv id
and the added municipality id.

``` r
match_yishuv_muni <- function(data, id_col_num){

  df_keys <- get_yishuv_muni() %>% 
    select(-nafa_id)
  
  data %>% 
    mutate(
      yishuv_id = as.character(pull(., id_col_num)),
      yishuv_id = str_pad(yishuv_id, width = 4, side = "left", pad = "0")
      ) %>% 
    left_join(df_keys, by = ("yishuv_id"))
  
}
```

## Manipulating elections data to fit municipalities data

The function receives a data frame of the elections with municipal id
for every voting site. It renames the relevant total and party-specific
variables, groups by every municipality, and calculates summary
statistics: voting percentage for HaLikud party as a part of total
votes, voting percentage for coalition parties (Israel Betetny was
included even though they left the coalition 1 year prior to elections),
total potential votes and total good votes. the function returns the
summarized data frame. NA’s are yishuvim not under any municipality, the
only municipality with no votes is Ein Kinya (muni_id 4502).

``` r
get_elect_pct <- function(data){
  
  data %>% 
    rename(
      pot_votes = 4,
      good_votes = 7,
      yahadut_hatorah = 9,
      habait_hayehudi = 14,
      kulanu = 19,
      israel_beytenu = 20,
      halikud = 21,
      shas = 33
    ) %>% 
    group_by(muni_id) %>% 
    summarize(
      likud_pct = 100 * sum(halikud) / sum(good_votes),
      coal_pct = 100 * sum(
        yahadut_hatorah +
        habait_hayehudi +
        kulanu +
        israel_beytenu +
        halikud +
        shas
      ) / sum(good_votes),
      pot_votes = sum(pot_votes),
      good_votes = sum(good_votes)
    )
}
```

# Budget data

## Importing and manipulating Sela budget data from Open Budget for 2016-2020

The function reads the csv file from [The Open Budget
website](https://next.obudget.org/i/budget/0019420256/2020?li=0&theme=budgetkey).
It then selects and renames relevant variables, replaces NA’s with 0’s,
summarizes by year and municipality, and filters for the relevant year
from the parameter. It returns the summarized data frame. Important to
note the current url method is pretty brutal and further programming is
needed here, maybe working with their API.

``` r
get_sela_data_open <- function(year_param){
  url <- "https://next.obudget.org/api/download?query=SELECT%20year_requested%20AS%20%22%D7%A9%D7%A0%D7%94%22%2C%20supporting_ministry%20AS%20%22%D7%9E%D7%A9%D7%A8%D7%93%22%2C%20request_type%20AS%20%22%D7%A1%D7%95%D7%92%20%D7%AA%D7%9E%D7%99%D7%9B%D7%94%22%2C%20support_title%20AS%20%22%D7%A0%D7%95%D7%A9%D7%90%22%2C%20budget_code%2C%20budget_code%20AS%20%22%D7%9E%D7%A1%D7%A4%D7%A8%20%D7%AA%D7%A7%D7%A0%D7%94%22%2C%20%27supports%2F%27%20%7C%7C%20budget_code%20%7C%7C%20%27%2F%27%20%7C%7C%20year_requested%20%7C%7C%20%27%2F%27%20%7C%7C%20entity_id%20%7C%7C%20%27%2F%27%20%7C%7C%20request_type%20AS%20item_id%2C%20coalesce(entity_name%2C%20recipient)%20as%20%22%D7%9E%D7%A7%D7%91%D7%9C%20%D7%94%D7%AA%D7%9E%D7%99%D7%9B%D7%94%22%2C%20entity_id%20as%20%22%D7%9E%D7%A1%D7%A4%D7%A8%20%D7%AA%D7%90%D7%92%D7%99%D7%93%22%2C%20%27org%2F%27%20%7C%7C%20entity_kind%20%7C%7C%20%27%2F%27%20%7C%7C%20entity_id%20as%20entity_item_id%2C%20sum(amount_approved)%20as%20%22%D7%A1%D7%94%D7%B4%D7%9B%20%D7%90%D7%95%D7%A9%D7%A8%22%2C%20sum(amount_paid)%20as%20%22%D7%A1%D7%94%D7%B4%D7%9B%20%D7%A9%D7%95%D7%9C%D7%9D%22%20FROM%20raw_supports%20WHERE%20year_requested%20%3E0%20AND%20budget_code%20%3D%20%270019420256%27%20GROUP%20BY%201%2C%202%2C%203%2C%204%2C%205%2C%206%2C%207%2C%208%2C%209%2C%2010%20order%20by%20year_requested%20desc&format=csv&filename=%D7%A1%D7%9C%20%D7%9C%D7%AA%D7%A8%D7%91%D7%95%D7%AA%20%D7%A2%D7%99%D7%A8%D7%95%D7%A0%D7%99%D7%AA__%20%D7%A4%D7%99%D7%A8%D7%95%D7%98%20%D7%9B%D7%9C%20%D7%94%D7%AA%D7%9E%D7%99%D7%9B%D7%95%D7%AA%20%D7%9E%D7%AA%D7%A7%D7%A6%D7%99%D7%91%20%D7%96%D7%94%20%D7%A9%D7%90%D7%95%D7%A9%D7%A8%D7%95%20%D7%91%20%D7%9B%D7%9C%20%D7%94%D7%A9%D7%A0%D7%99%D7%9D&headers=%D7%A0%D7%95%D7%A9%D7%90%3Aitem_link(item_id)%3B%D7%9E%D7%A1%D7%A4%D7%A8%20%D7%AA%D7%A7%D7%A0%D7%94%3Abudget_code%3Asearch_term(budget_code)%3B%D7%9E%D7%A9%D7%A8%D7%93%3B%D7%A1%D7%95%D7%92%20%D7%AA%D7%9E%D7%99%D7%9B%D7%94%3B%D7%9E%D7%A7%D7%91%D7%9C%20%D7%94%D7%AA%D7%9E%D7%99%D7%9B%D7%94%3Aitem_link(entity_item_id)%3B%D7%9E%D7%A1%D7%A4%D7%A8%20%D7%AA%D7%90%D7%92%D7%99%D7%93%3B%D7%A9%D7%A0%D7%94%3B%D7%A1%D7%94%D7%B4%D7%9B%20%D7%90%D7%95%D7%A9%D7%A8%3Anumber%3B%D7%A1%D7%94%D7%B4%D7%9B%20%D7%A9%D7%95%D7%9C%D7%9D%3Anumber"
  
  read_csv(url) %>% 
    select(
      tax_name = 5,
      tax_id = 6,
      year = 7,
      budget_approved = 8,
      budget_paid = 9
    ) %>% 
    replace_na(
      list(budget_approved = 0, budget_paid = 0)
    ) %>% 
    group_by(tax_id, year) %>% 
    summarise(
      budget_approved = sum(budget_approved),
      budget_paid = sum(budget_paid)
    ) %>% 
    ungroup() %>% 
    filter(year == year_param)

}
check2 <- get_sela_data_open(2018)
```

    Error in read_csv(url) %>% select(tax_name = 5, tax_id = 6, year = 7, : could not find function "%>%"

## Importing and manipulating Sela budget data from the Ministry of Culture for 2016-2019

This function reads the XLSX file from the ministry of culture official
website[^1], reads the specific SELA sheet, removes the first totals
row, selects and renames the relevant columns, pivots the data to a
longer format by year and budget type (initiatives, festivals or total),
replaces NA’s with 0’s, filters for the needed year by parameter, and
pivots back to the wider format to distinguish easily between different
budget types.

``` r
get_sela_data_culture <- function(year_param){
  url <- "https://www.gov.il/BlobFolder/generalpage/ministry_support/he/%D7%AA%D7%9E%D7%99%D7%9B%D7%95%D7%AA%20%D7%94%D7%9E%D7%A9%D7%A8%D7%93%20%D7%9C%D7%92%D7%95%D7%A4%D7%99%20%D7%AA%D7%A8%D7%91%D7%95%D7%AA%202016-2019.xlsx"
  
  file_ext <- str_extract(url, "[0-9a-z]+$")
  GET(url, write_disk(tf <- tempfile(fileext = file_ext)))
  df <- read_excel(tf, sheet = 39)
  
  df %>% 
    slice_tail(n = -1) %>% 
    select(
      tax_id = 1,
      tax_name = 2,
      init_2016 = 3,
      fest_2016 = 4,
      tot_2016 = 5,
      init_2017 = 7,
      fest_2017 = 8,
      tot_2017 = 9,
      init_2018 = 11,
      fest_2018 = 12,
      tot_2018 = 13,
      init_2019 = 15,
      fest_2019 = 16,
      tot_2019 = 17
    ) %>% 
    pivot_longer(-c(tax_id, tax_name), names_to = c("budget_type", "year"), names_sep = "_", values_to = "budget") %>% 
    replace_na(list(budget = 0)) %>% 
    mutate(
      tax_id = as.numeric(tax_id),
      year = as.numeric(year)
      ) %>% 
    filter(year == year_param) %>% 
    pivot_wider(c(tax_id, year), names_from = "budget_type", names_prefix = "budget_approved_", values_from = "budget")
}

check <- get_sela_data_culture(2018)
```

    Error in str_extract(url, "[0-9a-z]+$"): could not find function "str_extract"

## Getting conversion table between tax municipal id and CBS municipal id

This function reads the csv file from my GitHub repository, selects the
two relevant variables, and returns the data frame.

``` r
get_muni_id_conv <- function(){
  
  read_csv("https://raw.githubusercontent.com/matanhakim/general_files/main/muni_ids.csv") %>% 
  select(
    cbs_id,
    tax_id
  )  
}
```

## Adding municipality id to a data frame with budget data

``` r
match_budget_muni <- function(data){
  df_keys <- get_muni_id_conv()
  
  data %>% 
    left_join(df_keys, by = "tax_id") %>% 
    select(
      -tax_id,
      -year
    )
  
}
```

# SELA adiitional data

## 2013 CBS SES data

This function reads the 2013 CBS SES data for municipalities that is
being used by 2018 SELA regulations to determine eligibility of
municipalities. It reads the file, selects the relevant variables,
removes excess rows, and transforms the id to the usual format.

``` r
read_ses_2013 <- function(url){
  file_ext <- str_extract(url, "[0-9a-z]+$")
  GET(url, write_disk(tf <- tempfile(fileext = file_ext)))
  
  df <- read_excel(tf, skip = 6)
  
  df %>% 
    slice(2:256) %>%
    select(
      muni_status = 1,
      muni_id = 2,
      heb_name = 3,
      ses_2013_i = 5,
      ses_2013_r = 6,
      ses_2013_c = 7
    ) %>% 
    mutate(
      muni_id = as.character(muni_id),
      muni_id = case_when(
        (muni_status == "0" | muni_status == "99") ~ str_pad(muni_id, width = 4, side = "left", pad = "0"),
        TRUE ~ str_pad(muni_status, width = 2, side = "left", pad = "0")
      )
    ) %>% 
    select(-c(muni_status, heb_name))
   
}
```

## 2004 CBS periphery data

Important to note that this is and old indicator, therefore since then
some municipal jurisdiction changes have happened:

``` r
read_peri_2004 <- function(url){
  file_ext <- str_extract(url, "[0-9a-z]+$")
  GET(url, write_disk(tf <- tempfile(fileext = file_ext)))
  
  df <- read_excel(tf, skip = 7) 
  
  df %>% 
    select(
      muni_id = 1,
      heb_name = 2,
      peri_2004_i = 9,
      peri_2004_r = 10,
      peri_2004_c = 11
    ) %>% 
    mutate(
      muni_id = as.character(muni_id),
      muni_id = case_when(
        str_length(muni_id) == 5 ~ str_sub(muni_id, start = -2),
        TRUE ~ str_pad(muni_id, width = 4, side = "left", pad = "0")
      )
    ) %>% 
    select(-heb_name)
    
}
```

## 2008 CBS statistical areas SES data

``` r
read_ses_sa_2008 <- function(url){
  file_ext <- str_extract(url, "[0-9a-z]+$")
  GET(url, write_disk(tf <- tempfile(fileext = file_ext)))
  
  df <- read_excel(tf, skip = 5) 
  
  df %>% 
    slice_head(n = -3) %>% 
    select(
      muni_id = 2,
      sa_id = 4,
      ses_sa_2008_i = 7,
      ses_sa_2008_r = 8,
      ses_sa_2008_c = 9
    ) %>% 
    mutate(
      muni_id = str_pad(as.character(muni_id), width = 4, side = "left", pad = "0")
    ) %>% 
    nest(-muni_id) %>% 
    rename(sa_data = data)
}
```

## National priority settlements decided by the Israeli government

Since the SELA budget relies also on national priority areas, these data
are needed to be imported. \### Getting the list of tables from the
national priority webpage This function reads the table data in the
national priority areas government decision webpage, and returns a list
of those tables.

``` r
get_nat_pri_list <- function(){
  nat_pri_url <- "https://www.gov.il/he/departments/policies/2013_des667"
  
  read_html(nat_pri_url) %>%
    html_elements("table") %>% 
    html_table()
}
```

### Merging national priority yishuvim, subdistricts (Nafot) and yishuvim close to the border

This function reads the tables from the previous section and manipulates
them: - The nafot (subdistricts) data is added with the corresponding
nafa_id column, and converts Hebrew data to logical. - The yishuvim
declared as national pririty are cleaned, added with a TRUE column and
formats the yishuv_id. - The yishuvim declared as close to the border or
threatened are cleaned, added with a TRUE column and formats the
yishuv_id. - The whole yishuvim list is being caled from the CBS
website, and then all other three data frames are joined. NA’s are
replaced with FALSE, and a final national priority variable for each
yishuv is calculated. - Finally, yishuvim with NA as municipality are
filtered out, and a final national priority variable for each
municipality is calculated as having either more than 75% of yishuvim in
the municipality as national priority, or more than 50% of yishuvim in
the municipality as close to the border or threatened.

``` r
get_nat_pri_munis <- function(){
  
  pri_list <- get_nat_pri_list()
  
  pri_nafot <- pri_list[[1]] %>% 
    add_column(nafa_id = c(29,21,24,62,22,23,71,32,11,61,31,41,44,43,42,51)) %>% 
    mutate(
      nafa_id = as.character(nafa_id),
      nafa_nat_pri = (X7 == "כן")
      ) %>% 
    select(nafa_id, nafa_nat_pri)
  
  pri_yishuvim <- pri_list[[2]] %>% 
    select(yishuv_id = 1) %>% 
    slice_tail(n = -1) %>% 
    add_column(yishuv_nat_pri = TRUE) %>% 
    mutate(yishuv_id = str_pad(yishuv_id, width = 4, side = "left", pad = "0"))
  
  pri_border <- pri_list[[3]] %>% 
    select(yishuv_id = 1) %>% 
    slice_tail(n = -1) %>% 
    add_column(border_nat_pri = TRUE) %>% 
    mutate(yishuv_id = str_pad(yishuv_id, width = 4, side = "left", pad = "0"))
  
  pri_df <- get_yishuv_muni() %>% 
    left_join(pri_nafot, by = "nafa_id") %>% 
    left_join(pri_yishuvim, by = "yishuv_id") %>% 
    left_join(pri_border, by = "yishuv_id") %>% 
    mutate(
      across(ends_with("pri"), ~ replace_na(., FALSE)),
      is_nat_pri = nafa_nat_pri | yishuv_nat_pri | border_nat_pri
    )
    
  pri_df %>% 
  filter(!is.na(muni_id)) %>% 
  group_by(muni_id) %>% 
  summarise(is_nat_pri = (mean(is_nat_pri) > 0.75) | (mean(border_nat_pri) >= 0.5))
    
}
```

# Combining all data sources into one data frame

``` r
muni_2018_url <- "https://www.cbs.gov.il/he/publications/doclib/2019/hamakomiot1999_2017/2018.xlsx" # Initializing the url for 2018 municipalities data
elec_url <- "https://bechirot22.bechirot.gov.il/election/Documents/%D7%91%D7%97%D7%99%D7%A8%D7%95%D7%AA%20%D7%A7%D7%95%D7%93%D7%9E%D7%95%D7%AA/results_20.xls" # Initializing the url for 2015 elections
ses_2013_url <- "https://www.cbs.gov.il/he/publications/doclib/2017/socio_eco13_1694/t02.xls" # Initializing the url for 2013 CBS SES data
peri_2004_url <- "https://www.cbs.gov.il/he/mediarelease/doclib/2008/160/24_08_160t2.xls" # Initializing the url for 2004 CBS periphery data
ses_sa_2008_url <- "https://www.cbs.gov.il/he/mediarelease/doclib/2013/087/24_13_087t6.xls" # Initializing the url for 2008 CBS atatistical areas data
pop_2015_url <- "https://www.cbs.gov.il/he/publications/doclib/2019/hamakomiot1999_2017/2015.xls" # Initializing the url for 2015 CBS population data

muni_df <- read_muni_new(muni_2018_url) %>% 
  rename(muni_id = 2)
```

    Error in read_muni_new(muni_2018_url) %>% rename(muni_id = 2): could not find function "%>%"

``` r
elec_df <- read_elec_general(elec_url) %>%  # Reading the general elections raw data
  match_yishuv_muni(2) %>% # Adding the linked yishuvim and municipalities to elections data
  get_elect_pct() # Calculating elections data by municipality
```

    Error in read_elec_general(elec_url) %>% match_yishuv_muni(2) %>% get_elect_pct(): could not find function "%>%"

``` r
budget_open_df <- get_sela_data_open(2018) %>% # Reading and manipulating Sela open budget data by year and municipality
  match_budget_muni() # Adding the cbs municipality id
```

    Error in get_sela_data_open(2018) %>% match_budget_muni(): could not find function "%>%"

``` r
budget_culture_df <- get_sela_data_culture(2018) %>% # Reading and manipulating Sela ministry of culture budget data by year and municipality
  match_budget_muni() # Adding the cbs municipality id
```

    Error in get_sela_data_culture(2018) %>% match_budget_muni(): could not find function "%>%"

``` r
ses_2013_df <- read_ses_2013(ses_2013_url) # Reading 2013 CBS SES data
```

    Error in str_extract(url, "[0-9a-z]+$"): could not find function "str_extract"

``` r
peri_2004_df <- read_peri_2004(peri_2004_url) # Reading 2004 CBS periphery data
```

    Error in str_extract(url, "[0-9a-z]+$"): could not find function "str_extract"

``` r
ses_sa_2008_df <- read_ses_sa_2008(ses_sa_2008_url) # Reading 2008 CBS statistical areas data
```

    Error in str_extract(url, "[0-9a-z]+$"): could not find function "str_extract"

``` r
nat_pri_df <- get_nat_pri_munis() # Reading 2013 national priority areas government decision for municipalities
```

    Error in read_html(nat_pri_url) %>% html_elements("table") %>% html_table(): could not find function "%>%"

``` r
pop_2015_df <- read_muni_old(pop_2015_url, 14, 31) %>% 
  rename(pop_2015 = var)# Reading the 2015 CBS population data
```

    Error in read_muni_old(pop_2015_url, 14, 31) %>% rename(pop_2015 = var): could not find function "%>%"

``` r
raw_df <- muni_df %>% 
  left_join(elec_df, by = "muni_id") %>% 
  left_join(budget_open_df, by = c("muni_id" = "cbs_id")) %>%
  left_join(budget_culture_df, by = c("muni_id" = "cbs_id")) %>% 
  mutate( # Change NA's (unbudgeted or no voters municipalities) to 0's
    across(
      last_col(5:0),
      replace_na, 0
    )
  ) %>% 
  left_join(ses_2013_df, by = "muni_id") %>% 
  left_join(peri_2004_df, by = "muni_id") %>% 
  left_join(ses_sa_2008_df, by = "muni_id") %>% 
  left_join(nat_pri_df, by = "muni_id") %>% 
  left_join(pop_2015_df, by = "muni_id")
```

    Error in muni_df %>% left_join(elec_df, by = "muni_id") %>% left_join(budget_open_df, : could not find function "%>%"

# Future Code that is not operatable right now

## Binding municipality files for 2016-2019 by row

Right now there is an error

``` r
bind_muni_2016_2019 <- function(){
  url_2016 <- "https://www.cbs.gov.il/he/publications/doclib/2019/hamakomiot1999_2017/2016.xlsx"
  url_2017 <- "https://www.cbs.gov.il/he/mediarelease/doclib/2019/057/24_19_057t1.xlsx"
  url_2018 <- "https://www.cbs.gov.il/he/publications/doclib/2019/hamakomiot1999_2017/2018.xlsx"
  url_2019 <- "https://www.cbs.gov.il/he/publications/doclib/2019/hamakomiot1999_2017/2019.xlsx"
  
  df_2016 <- read_muni_new(url_2016)
  df_2017 <- read_muni_new(url_2017)
  df_2018 <- read_muni_new(url_2018)
  df_2019 <- read_muni_new(url_2019)
  
  muni_df <- bind_rows(
    df_2016,
    df_2017#,
#    df_2018,
#    df_2019
    )
  
  muni_df
  
}
```

## Import a single municipalities file (2015 and earlier)

I read the municipalities file by url, extracts the two tables of local
municipalities and regional councils, and merges them. Right now there
is an error because of the merging process.

``` r
read_muni_old <- function(url){
  file_ext <- str_extract(url, "[0-9a-z]+$")
  GET(url, write_disk(tf <- tempfile(fileext = file_ext)))
  
  df2 <- read_excel(tf, sheet = 2, skip = 1)
  df2 <- df2 %>% drop_na(3)
  
  df3 <- read_excel(tf, sheet = 4, skip = 1)
  df3 <- df3 %>% drop_na(3)
  
  df <- bind_rows(df2, df3)
  return(df)

}

# df <- read_muni_old("https://www.cbs.gov.il/he/publications/doclib/2019/hamakomiot1999_2017/2015.xls")
```

[^1]: https://www.gov.il/he/departments/general/ministry_support
