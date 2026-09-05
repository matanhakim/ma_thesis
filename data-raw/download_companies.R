# Download a snapshot of the Israeli Companies Registrar ("רשם החברות") data
# from the Israeli government open-data portal (data.gov.il).
#
# The thesis (Hakim, 2024) matched every company supported by the Ministry of
# Culture to its registered locality ("שם עיר") using this registry. The
# original snapshot used for the thesis (downloaded in 2022) was not preserved,
# so this script re-creates the snapshot from the live CKAN datastore and keeps
# only the columns needed for the analysis. The full registry holds ~730,000
# companies and ~30 columns (about 250 MB), far too large to version in git.
#
# Source:
#   Dataset:  https://data.gov.il/dataset/ica_companies
#   Resource: f004176c-b85f-4542-8901-7b3176f9a054 ("רשימת החברות")
#   API:      CKAN datastore_search, paged 32,000 records at a time
#
# Output: data/raw/organizations/companies_registry.csv.gz
#
# Usage: Rscript data-raw/download_companies.R

library(httr2)
library(dplyr)
library(purrr)
library(readr)

resource_id <- "f004176c-b85f-4542-8901-7b3176f9a054"
fields <- c("מספר חברה", "שם חברה", "סטטוס חברה", "שם עיר", "קוד ישוב")
page_size <- 32000L
out_path <- file.path("data", "raw", "organizations", "companies_registry.csv.gz")

fetch_page <- function(offset) {
  request("https://data.gov.il/api/3/action/datastore_search") |>
    req_url_query(
      resource_id = resource_id,
      fields = paste(fields, collapse = ","),
      limit = page_size,
      offset = offset
    ) |>
    req_user_agent("ma_thesis reproducibility script (https://github.com/matanhakim/ma_thesis)") |>
    req_retry(max_tries = 5) |>
    req_perform() |>
    resp_body_json()
}

first <- fetch_page(0L)
total <- first$result$total
message("Total records in registry: ", format(total, big.mark = ","))
offsets <- seq(0L, total - 1L, by = page_size)

records_to_tibble <- function(page) {
  page$result$records |>
    map(\(rec) map(rec, \(x) if (is.null(x)) NA_character_ else as.character(x))) |>
    bind_rows()
}

pages <- map(offsets, \(off) {
  message("  offset ", format(off, big.mark = ","))
  if (off == 0L) first else fetch_page(off)
}, .progress = FALSE)

companies <- pages |>
  map(records_to_tibble) |>
  list_rbind() |>
  select(
    company_id = `מספר חברה`,
    company_name = `שם חברה`,
    company_status = `סטטוס חברה`,
    city_name = `שם עיר`,
    city_code = `קוד ישוב`
  ) |>
  distinct()

stopifnot(nrow(companies) >= total * 0.99)
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
write_csv(companies, out_path, na = "")
message("Wrote ", format(nrow(companies), big.mark = ","), " rows to ", out_path,
        " (", round(file.size(out_path) / 1e6, 1), " MB)")
writeLines(
  c(
    paste0("snapshot_date: ", format(Sys.Date())),
    paste0("resource_id: ", resource_id),
    paste0("records: ", nrow(companies)),
    paste0("sha256: ", digest::digest(out_path, algo = "sha256", file = TRUE))
  ),
  sub(".csv.gz", ".snapshot.txt", out_path, fixed = TRUE)
)
