# Data

Every file the analysis reads is kept in this directory, so that the thesis
can be rebuilt without any network access. `data/raw/` holds the source files
exactly as obtained from their publishers (only the file names were changed to
ASCII), and `data/reference/` holds small lookup tables that the analysis
depends on. Nothing in `data/` is ever modified by the pipeline; derived data
lives in the `_targets/` store, which `targets::tar_make()` rebuilds.

All amounts are in NIS. Identifiers are stored as text because several of them
carry leading zeros.

## Raw data (`data/raw/`)

| File | Content | Publisher and original file | Used for |
|---|---|---|---|
| `budget/culture_administration_supports_2009_2021.csv` | Every support approved from the Culture Administration budget items, 2009-2021, by recipient organisation (tax id), year, approved and paid amounts | Open Budget ("מפתח התקציב", <https://next.obudget.org>), export "מינהל התרבות: פירוט כל התמיכות מתקציב זה שאושרו בכל השנים", downloaded 2023 | Culture budget of every organisation, placed in its local authority (2013-2019) |
| `budget/moc_supports_by_regulation_2016_2019.xlsx` | Ministry of Culture and Sport supports by regulation and recipient, 2016-2019; sheet `סל"ע 42-02-56` lists the SELA grants (initiatives and festivals tracks) to every municipality | Ministry of Culture and Sport, "תמיכות המשרד בגופי תרבות לשנים 2016-2019" (freedom-of-information release, 2022); original name `תמיכות המשרד לגופי תרבות 2016-2019.xlsx` | SELA budget per municipality; the hypothetical budget without SELA; calibration of the 2018 initiatives total |
| `elections/knesset_20_results_by_locality_2015.xls` | Results of the elections to the 20th Knesset (17 March 2015) by locality: eligible voters, valid votes and votes per list | Central Elections Committee (<https://votes20.bechirot.gov.il>), file `results_20.xls` | Likud vote share of every local authority |
| `indices/cbs_socioeconomic_index_2013_t2.xls` | Socio-economic index of local authorities 2013: index value, rank and cluster (1-10) | Central Bureau of Statistics (CBS), "רשויות מקומיות, לפי סדר עולה של המדד החברתי-כלכלי 2013", table 2 (`t02.xls`, published 2017) | Socio-economic cluster used by the 2018 SELA support tests |
| `indices/cbs_peripherality_index_2004_t2.xls` | Peripherality index of local authorities 2004: index value, rank and cluster (1-10) | CBS, "דירוג רשויות מקומיות לפי מדד הפריפריאליות 2004", table 2 (`24_08_160t2.xls`, published 2008) | Peripherality cluster used by the 2018 SELA support tests |
| `municipalities/cbs_local_authorities_2013.xls` … `_2019.xlsx` | The annual CBS "Local Authorities in Israel" data files. Pre-2016 files hold cities/local councils and regional councils on separate sheets and report population in thousands; so do the 2016 and 2017 files | CBS, "הרשויות המקומיות בישראל - קובץ נתונים לעיבוד", one file per year (`2013.xls` … `2019.xlsx`) | Population of every authority and year; municipality type, share of Arab residents (2019) and the 2015 peripherality index (from the 2016 file) |
| `national_priority/nafot_priority.csv` | The sub-district table of Government Decision 667 (national priority areas), with the "national priority" decision (כן/לא) in column 7 | Prime Minister's Office, decision 667 of 4 August 2013 (<https://www.gov.il/he/departments/policies/2013_des667>); the three CSV files are the tables of that page as scraped in 2022 | Which local authorities count as national priority areas |
| `national_priority/yishuvim_border.csv` | Localities listed as adjacent to the border or under threat | Same decision | Same |
| `national_priority/yishuvim_priority.csv` | Localities listed individually as national priority localities | Same decision | Same |
| `organizations/guidestar_monthly_report_2023_05.xlsx` | Registered non-profits (עמותות) with their registered city, May 2023 monthly report (data on sheet 2) | GuideStar Israel (<https://www.guidestar.org.il>), "דוח חודשי גיידסטאר" | Locality of every supported non-profit |
| `organizations/guidestar_report_2020_08.xlsx` | Registered non-profits, August 2020 report; keeps organisations dissolved since | GuideStar Israel, "דוח גיידסטאר - אוגוסט 2020" | Same, for organisations missing from the 2023 report |
| `organizations/companies_registry.csv.gz` | Companies Registrar: company number, name, status, registered city and city code, all 730,574 companies | data.gov.il, dataset "רשם החברות", resource `f004176c-b85f-4542-8901-7b3176f9a054`; snapshot of 5 September 2026 produced by `data-raw/download_companies.R` (see `companies_registry.snapshot.txt` for the checksum) | Locality of every supported company |
| `yishuvim/cbs_localities_2021_bycode.xlsx` | The CBS localities file 2021: every locality with its code, sub-district and municipal status (city, local council, or the code of its regional council) | CBS, "רשימת היישובים" (`bycode2021.xlsx`) | Mapping localities to local authorities; sub-districts for the national priority rule |

### A note on the Companies Registrar snapshot

The thesis matched companies to their locality with a Companies Registrar
extract downloaded in 2022. That extract was not preserved. The snapshot in
this repository was rebuilt from the live registry in September 2026, keeping
only the columns the analysis needs (the full registry is about 250 MB). A
handful of companies have changed address or lost their address since 2022,
which moves a few hundred thousand NIS between authorities in some years. The
effect on the published results is limited to the third decimal of the Gini
coefficient in 2017 and 2018 and to the 2017 total budget label (698 rather
than 699 million NIS); every other figure and table reproduces the thesis
exactly (see `output/comparison_report.md` after a build).

## Reference tables (`data/reference/`)

| File | Content | Source |
|---|---|---|
| `muni_ids.csv` | The 255 local authorities of 2013-2019 with the identifiers of three agencies: CBS (`cbs_id`, the `muni_id` used throughout), Ministry of Education (`edu_id`) and Tax Authority (`tax_id`), and the name each agency uses | Frozen copy of `muni_ids.csv` from <https://github.com/matanhakim/general_files> at commit `2af51573` (25 August 2022). Later versions of that file, and `il.cbs.muni::read_muni_id()`, list 257 authorities and must not be used here |
| `yishuv_names.csv` | Every spelling of a locality name found in the organisation registries, mapped to the CBS locality code (6,312 rows). Regional councils appear with their two-digit code | Frozen copy of `yishuv_names.csv` from the same repository at commit `79a8800f` (3 June 2023) |
| `organizations_manual_yishuv.csv` | 25 supported bodies (universities, orchestras, companies) whose registered locality is missing from both registries, with the locality assigned by hand from public records | Compiled for the thesis in 2023 |

## Identifiers

* `muni_id` is the CBS code of a local authority: the four-digit locality code
  for cities and local councils (for example `5000` Tel Aviv-Yafo, `0472` Abu
  Ghosh) and the two-digit council code for regional councils (for example
  `25` Hevel Modi'in).
* `yishuv_id` is the four-digit CBS locality code.
* `tax_id` is the nine-digit registration number of an organisation at the
  Tax Authority (companies start with 51/52, non-profits with 58,
  municipalities with 50).

## Terms of use

The raw files are the property of their publishers and are redistributed here
for the sole purpose of reproducing the thesis. CBS and Central Elections
Committee files are public sector information; the Open Budget export and the
Companies Registrar snapshot come from open government data portals; the
GuideStar reports were provided for research use. The code of this repository
is released under the MIT licence (see `LICENSE`); the data are not.
