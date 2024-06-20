devtools::load_all()
library(dplyr, warn.conflicts = FALSE)
options(timeout = max(2500, getOption("timeout")), download.file.method = "libcurl")

install_cagis_data <- function(cagis_data_url) {
  cagis_gdb_name <- tools::file_path_sans_ext(basename(cagis_data_url))
  dest <- file.path(tools::R_user_dir(package = "addr", "data"), cagis_gdb_name)
  if (file.exists(dest)) {
    return(dest)
  }
  tmp <- tempfile(fileext = ".zip")
  utils::download.file(cagis_data_url, destfile = tmp, mode = "wb")
  unzip(tmp, exdir = dirname(dest))
  return(dest)
}

ca <-
  install_cagis_data("https://www.cagis.org/Opendata/Quarterly_GIS_Data/CAGISOpenDataSpring2024.gdb.zip") |>
  sf::st_read(layer = "Addresses") |>
  sf::st_drop_geometry() |>
  select(PARCELID, FULLMAILADR, LATITUDE, LONGITUDE) |>
  transmute(
    parcel_id = PARCELID,
    parcel_address = FULLMAILADR,
    s2 = s2::as_s2_cell(s2::s2_geog_point(LONGITUDE, LATITUDE))
  ) |>
  tibble::as_tibble()

ca_addr <-
  ca$parcel_address |>
  addr_standardize() |>
  purrr::transpose() |>
  tibble::as_tibble() |>
  mutate(across(everything(), as.character))

ca <- purrr::list_cbind(list(ca, ca_addr))

# read in messy real-world addresses
d <-
  readr::read_csv(
    "../riseup_geomarker_pipeline/data/DR1767_r2.csv",
    na = c("NA", "-", "NULL", "null"),
    col_types = readr::cols_only(
      MRN = readr::col_character(),
      PAT_ENC_CSN_ID = readr::col_character(),
      ADDRESS = readr::col_character(),
      CITY = readr::col_character(),
      STATE = readr::col_character(),
      ZIP = readr::col_character()
    )
  ) |>
  filter(substr(ZIP, 1, 5) %in% cincy::zcta_tigris_2020$zcta_2020) |>
  tidyr::unite("raw_address", c(ADDRESS, CITY, STATE, ZIP), sep = " ", na.rm = TRUE)

d_addr <-
  d$raw_address |>
  addr_standardize() |>
  purrr::transpose() |>
  tibble::as_tibble() |>
  mutate(across(everything(), as.character))

d <- purrr::list_cbind(list(d, d_addr))

library(fuzzyjoin)

# TODO calculate stringsim based on number of edits (faster method than osm??)
# TODO use number of edits for zipcode first to narrow it down, then use osm + soundex for streetname and number as identical

the_join <-
  fuzzy_join(sample_n(d, 100), ca,
  by = c("AddressNumber", "StreetName", "ZipCode"),
  match_fun = list(
    AddressNumber = \(.x, .y) as.integer(.x) == as.integer(.y),
    StreetName = \(.x, .y) stringdist::stringdist(.x, .y, method = "osa") > 0.5,
    ZipCode = \(.x, .y) .x == .y
  ),
  mode = "left"
)

ca[
  purrr::map_dbl(
    d$StreetName[1:100],
    \(.) which.max(stringdist::stringsim(tolower(.), tolower(ca$StreetName), method = "osa")),
    .progress = TRUE
  ),
]

ca[30, ]
