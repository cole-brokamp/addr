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

d_cagis_address <-
  sf::st_read(
    dsn = install_cagis_data("https://www.cagis.org/Opendata/Quarterly_GIS_Data/CAGISOpenDataSpring2024.gdb.zip"),
    layer = "Addresses"
  ) |>
  sf::st_drop_geometry() |>
  select(PARCELID, FULLMAILADR, LATITUDE, LONGITUDE) |>
  transmute(
    parcel_id = PARCELID,
    parcel_address = FULLMAILADR,
    s2 = s2::as_s2_cell(s2::s2_geog_point(LONGITUDE, LATITUDE))
  ) |>
  tibble::as_tibble()

d_cagis_address$parcel_addr <- addr_standardize(d_cagis_address$parcel_address, collapse = FALSE)

pryr::object_size(d_cagis_address)

d_cagis_address$parcel_addr

purrr::modify(as.data.frame) |>
  purrr::list_rbind()










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
  )

d_parcel <-
  d |>
  filter(substr(ZIP, 1, 5) %in% cincy::zcta_tigris_2020$zcta_2020) |>
  tidyr::unite("raw_address", c(ADDRESS, CITY, STATE, ZIP), sep = " ", na.rm = TRUE)
