# Readers for the Israeli Central Bureau of Statistics (CBS) files: the annual
# "Local Authorities in Israel" data files, the socio-economic and
# peripherality indices, and the localities (yishuvim) file.
#
# The layout of the CBS Excel files changes from year to year (multi-row
# headers, merged cells, separate sheets for regional councils before 2016).
# The `il.cbs.muni` package handles those quirks; the functions below only add
# the column positions and unit conversions specific to this thesis.

#' Column positions of the population variable in the CBS local-authority files
#'
#' Positions are 1-indexed data columns after the package strips the header
#' rows. Files up to 2015 hold cities/local councils and regional councils on
#' two separate sheets, each with its own column position. Population is
#' reported in thousands up to 2017 and in persons from 2018.
cbs_population_columns <- function() {
  tibble::tribble(
    ~year, ~col_city, ~col_rc, ~in_thousands,
    2013L, 15L, 30L, TRUE,
    2014L, 16L, 33L, TRUE,
    2015L, 14L, 31L, TRUE,
    2016L, 13L, NA_integer_, TRUE,
    2017L, 13L, NA_integer_, TRUE,
    2018L, 13L, NA_integer_, FALSE,
    2019L, 13L, NA_integer_, FALSE
  )
}

#' Read the population of every local authority for every year 2013-2019
#'
#' @param paths Paths to the CBS local-authority files. The year is taken from
#'   the four digits in each file name.
#' @return A tibble with columns year, muni_id, pop (persons).
read_population <- function(paths) {
  spec <- tibble::tibble(
    path = paths,
    year = as.integer(stringr::str_extract(basename(paths), "\\d{4}"))
  ) |>
    dplyr::inner_join(cbs_population_columns(), dplyr::join_by(year))
  stopifnot(nrow(spec) == length(paths))

  read_one <- function(path, year, col_city, col_rc, in_thousands) {
    data <- if (year <= 2015) {
      il.cbs.muni::combine_cbs_muni(
        path, year,
        cols_city = c(2, col_city), cols_rc = c(2, col_rc),
        col_names = c("muni_id", "pop")
      )
    } else {
      il.cbs.muni::read_cbs_muni(
        path, year,
        cols = c(2, col_city), col_names = c("muni_id", "pop")
      )
    }
    data |>
      dplyr::mutate(
        year = year,
        pop = readr::parse_number(pop) * if (in_thousands) 1000 else 1
      )
  }

  purrr::pmap(spec, read_one) |>
    purrr::list_rbind() |>
    dplyr::select(year, muni_id, pop)
}

#' Classify every local authority as Arab or Jewish by its 2019 population share
#'
#' A local authority is "arab" when more than 50% of its residents are Arab.
#'
#' @param path Path to the CBS local-authority file for 2019.
#' @return A tibble with columns muni_id, sector (factor: arab, jewish).
read_sector <- function(path) {
  il.cbs.muni::read_cbs_muni(
    path, 2019L,
    cols = c(2, 16), col_names = c("muni_id", "arab_pct")
  ) |>
    dplyr::mutate(
      arab_pct = readr::parse_number(stringr::str_replace(arab_pct, "^-$", "0")),
      sector = factor(
        dplyr::if_else(arab_pct > 50, "arab", "jewish"),
        levels = c("arab", "jewish")
      )
    ) |>
    dplyr::select(muni_id, sector)
}

#' Read municipality type and the 2015 peripherality index from the 2016 CBS file
#'
#' The 2016 local-authority file is the first to carry the 2015 peripherality
#' index (value, rank and cluster) alongside the municipal status.
#'
#' @param path Path to the CBS local-authority file for 2016.
#' @return A tibble with columns muni_id, muni_type (Hebrew label),
#'   peri_2015_c (cluster), peri_2015_i (index value), peri_2015_r (rank).
read_muni_type_peri_2015 <- function(path) {
  il.cbs.muni::read_cbs_muni(
    path, 2016L,
    cols = c(2, 4, 187, 188, 189),
    col_names = c("muni_id", "muni_type", "peri_2015_c", "peri_2015_i", "peri_2015_r")
  ) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("peri_"), readr::parse_number))
}

#' Read the 2013 CBS socio-economic index of local authorities
#'
#' Used by the 2018 SELA regulations to determine eligibility.
#'
#' @param path Path to CBS table 2 of the 2013 socio-economic index.
#' @return A tibble with columns muni_id, ses_2013_i (index value),
#'   ses_2013_r (rank), ses_2013_c (cluster 1-10).
read_ses_2013 <- function(path) {
  il.cbs.muni::read_cbs_index(path, year = 2013L, index_type = "ses", quiet = TRUE) |>
    dplyr::select(
      status = 1, yishuv_id = 2,
      ses_2013_i = 5, ses_2013_r = 6, ses_2013_c = 7
    ) |>
    dplyr::mutate(
      yishuv_id = il.cbs.muni::pad_yishuv_id(yishuv_id),
      muni_id = il.cbs.muni::modify_muni_id(status, yishuv_id),
      dplyr::across(dplyr::starts_with("ses_"), readr::parse_number)
    ) |>
    dplyr::select(muni_id, ses_2013_i, ses_2013_r, ses_2013_c)
}

#' Read the 2004 CBS peripherality index of local authorities
#'
#' Used by the 2018 SELA regulations. The file predates several municipal
#' mergers and splits, so a few authorities have no 2004 value (see
#' `build_panel()`). Regional councils appear with a five-digit code
#' (10000 + code); cities and local councils with their locality code.
#'
#' @param path Path to CBS table 2 of the 2004 peripherality index.
#' @return A tibble with columns muni_id, peri_2004_i, peri_2004_r, peri_2004_c.
read_peri_2004 <- function(path) {
  readxl::read_excel(path, skip = 7, col_names = FALSE, .name_repair = "minimal") |>
    dplyr::select(muni_id = 1, peri_2004_i = 9, peri_2004_r = 10, peri_2004_c = 11) |>
    dplyr::filter(!is.na(muni_id)) |>
    dplyr::mutate(
      muni_id = as.character(as.integer(muni_id)),
      muni_id = dplyr::if_else(
        stringr::str_length(muni_id) == 5,
        stringr::str_sub(muni_id, start = -2),
        stringr::str_pad(muni_id, width = 4, side = "left", pad = "0")
      ),
      dplyr::across(dplyr::starts_with("peri_"), as.numeric)
    )
}

#' Map every CBS locality (yishuv) to its local authority
#'
#' Column 9 of the localities file holds the municipal status: 0 for a city,
#' 99 for a local council (the locality is its own authority) and a two-digit
#' code for the regional council the locality belongs to. Localities outside
#' any authority (unrecognised Bedouin villages, etc.) get NA.
#'
#' @param path Path to the CBS localities file (bycode).
#' @return A tibble with columns yishuv_id, muni_id.
read_yishuv_muni <- function(path) {
  il.cbs.muni::read_cbs_yishuv(path, cols = c(2, 9), col_names = c("yishuv_id", "status")) |>
    dplyr::mutate(
      yishuv_id = il.cbs.muni::pad_yishuv_id(yishuv_id),
      muni_id = il.cbs.muni::modify_muni_id(status, yishuv_id)
    ) |>
    dplyr::select(yishuv_id, muni_id)
}

#' Read the sub-district (nafa) of every CBS locality
#'
#' @param path Path to the CBS localities file (bycode).
#' @return A tibble with columns yishuv_id, nafa_id.
read_yishuv_nafa <- function(path) {
  il.cbs.muni::read_cbs_yishuv(path, cols = c(2, 6), col_names = c("yishuv_id", "nafa_id")) |>
    dplyr::mutate(yishuv_id = il.cbs.muni::pad_yishuv_id(yishuv_id))
}
