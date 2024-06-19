devtools::load_all()
library(dplyr, warn.conflicts = FALSE)

tags_for_hashdress <- c("AddressNumber", "StreetName", "StreetNamePostType")

# load reference addresses from CAGIS
if (!fs::file_exists(fs::path_package("addr", "CAGISOpenDataSpring2024.gdb"))) {
  # TODO fix the conditional for the if statement (doesn't work on first try w/o pre-downloaded file)
  tmp <- tempfile(fileext = ".zip")
  download.file("https://www.cagis.org/Opendata/Quarterly_GIS_Data/CAGISOpenDataSpring2024.gdb.zip", tmp, timeout = 1000, method = "wget")
  unzip(tmp, exdir = fs::path_package("addr"))
}
d_ref <-
  sf::st_read(fs::path_package("addr", "CAGISOpenDataSpring2024.gdb"), layer = "Addresses") |>
  sf::st_drop_geometry() |>
  select(PARCELID, ADDRESS, LATITUDE, LONGITUDE) |>
  transmute(
    parcel_id = PARCELID,
    parcel_address = ADDRESS,
    s2 = s2::as_s2_cell(s2::s2_geog_point(LONGITUDE, LATITUDE))
  ) |>
  tibble::as_tibble()

d_ref$hashdress <- addr_hash(d_ref$parcel_address, tags = tags_for_hashdress)

## # add supplemental reference addresses
## d_supp_ref <-
##   tibble::tribble(
##     ~parcel_address, ~parcel_id,
##     "1302 WALNUT ST", "0800002019300",
##     "334 NORTHERN AVE", "011300040026",
##     "2743 TOWNTERRACE DR", "051000430254"
##   )
## d_ref <-
##   dplyr::bind_rows(d_ref, d_supp_ref) |>
##   mutate(hashdress = addr_hash(parcel_address, tags = tags_for_hashdress))
## tail(d_ref)

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

out <-
  d_parcel |>
  mutate(hashdress = addr_hash(raw_address, tags = tags_for_hashdress)) |>
  left_join(d_ref, by = "hashdress", relationship = "many-to-many")

out |>
  group_by(is.na(parcel_id)) |>
  summarize(n = n())

# matching 92% of addresses in all encounters!

# print all addresses for 5 randomly selected individuals
out |>
  nest_by(MRN) |>
  ungroup() |>
  sample_n(5) |>
  tidyr::unnest(cols = c(data)) |>
  knitr::kable()

# notable examples
## SUNNYWOOD LN -> SUNNYWOODS LN
## WINTHRIP DR -> WINTHROP DR
## DECAMP ST -> DE CAMP AVE
## PIPPEN RD -> PIPPIN RD

# TODO how to allow hashdresses to vary by one edit distance?
# add manually for all mispellings using supplemental data?
# allow only the street name (before or after suffix expansion?) to vary by edit distance of 1? require soft match for ZIP CODE, and exact match for street number
# use ZIP CODE field!!!!
