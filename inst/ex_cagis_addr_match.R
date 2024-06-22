devtools::load_all()
library(dplyr, warn.conflicts = FALSE)
options(timeout = max(2500, getOption("timeout")), download.file.method = "libcurl")

source("inst/addr.R")

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

# read in cagis addresses:
# - use only addresses that have `STATUS` of `ASSIGNED` or `USING` and are not orphaned (`ORPHANFLG == "N"`)
# - omit addresses with `ADDRTYPE`s that are milemarkers (`MM`), parks (`PAR`), infrastructure projects (`PRJ`),
#   cell towers (`CTW`), vacant or commercial lots (`LOT`), and other miscellaneous non-residential addresses (`MIS`, `RR`, `TBA`)
# - s2 cell derived from LONGITUDE and LATITUDE fields in CAGIS address database
ca <-
  install_cagis_data("https://www.cagis.org/Opendata/Quarterly_GIS_Data/CAGISOpenDataSpring2024.gdb.zip") |>
  sf::st_read(layer = "Addresses") |>
  sf::st_drop_geometry() |>
  filter(STATUS %in% c("ASSIGNED", "USING")) |>
  filter(ORPHANFLG == "N") |>
  filter(!ADDRTYPE %in% c("MM", "PAR", "PRJ", "CTW", "LOT", "MIS", "RR", "TBA")) |>
  transmute(
    address = FULLMAILADR,
    address_place = BLDGPLACE,
    address_type = ADDRTYPE,
    s2 = s2::as_s2_cell(s2::s2_geog_point(LONGITUDE, LATITUDE)),
    parcel_id = PARCELID,
    is_condo = CONDOFLG %in% c("Y")
  ) |>
  tibble::as_tibble()

ca$addr <- addr(ca$address)


# add standardized tags columns to cagis addresses
ca_addr <-
  ca$address |>
  addr_standardize() |>
  purrr::transpose() |>
  tibble::as_tibble() |>
  mutate(across(everything(), as.character))
ca <- purrr::list_cbind(list(ca, ca_addr))

# read in messy real-world addresses and create cleaned address in one string
# TODO add back in date for looking at the end (and for residential addr structure function??)
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
  tidyr::unite("raw_address", c(ADDRESS, CITY, STATE, ZIP), sep = " ", na.rm = TRUE) |>
  mutate(clean_address = clean_address_text(raw_address))

# create distinct raw addresses to create 'lookup' for merging back into all raw addresses in data
# add in standardized address tags
d_addr <-
  unique(d$clean_address) |>
  addr_standardize() |>
  purrr::transpose() |>
  tibble::as_tibble() |>
  mutate(across(everything(), as.character)) |>
  mutate(clean_address = unique(d$clean_address))
# remove raw addresses with parsed ZIP code not in CAGIS addresses parsed ZIP codes
d_addr <- filter(d_addr, ZipCode %in% ca$ZipCode)

message("Found ", prettyNum(nrow(d_addr), ","), " unique addresses with CAGIS address ZIP Codes in ", prettyNum(nrow(d), ","), " total address observations.")

# working in groupings defined by ZipCode, return list of matches for addresses; each item is a tibble, with one row per match
all_matches_by_zip <-
  vector("list", length = length(d_addr)) |>
  setNames(names(d_addr))

message("Processing unique address observations in ", length(d_addr), " ZIP Code groupings.")
tictoc::tic("Finished processing all ZIP codes")
local({
  ca <- split(ca, ~ZipCode)
  d_addr <- split(d_addr, ~ZipCode)
  for (zc in names(d_addr)) {
    tictoc::tic(paste("[[", zc, "]]: matching", prettyNum(nrow(d_addr[[zc]]), ","), "to", prettyNum(nrow(ca[[zc]]), ",")))
    street_matches <-
      stringdist::stringdistmatrix(d_addr[[zc]]$StreetName, ca[[zc]]$StreetName) |>
      apply(MARGIN = 1, FUN = \(.) which(. <= 1))
    number_matches <-
      stringdist::stringdistmatrix(d_addr[[zc]]$AddressNumber, ca[[zc]]$AddressNumber) |>
      apply(MARGIN = 1, FUN = \(.) which(. <= 0))
    # don't try in special case where no matches are found for any input address in a zipcode group
    if (!length(street_matches) == 0 & !length(number_matches) == 0) {
      all_matches_by_zip[[zc]] <<-
        purrr::map2(street_matches, number_matches, intersect) |>
        purrr::map(\(.) ca[[zc]][., c("address", "address_place", "is_condo", "address_type", "s2", "parcel_id", "ZipCode")]) |>
        setNames(d_addr[[zc]]$clean_address)
    }
    tictoc::toc()
  }
})
tictoc::toc()

# do not return match if no matching candidates, address `is_condo`
# for all other multiple matches, take the first row
all_matches <-
  all_matches_by_zip |>
  purrr::compact() |>
  purrr::list_flatten(name_spec = "{inner}", name_repair = "check_unique") |>
  purrr::discard(\(.) nrow(.) == 0) |>
  purrr::discard(\(.) any(.$is_condo)) |>
  purrr::modify_if(\(.) nrow(.) > 1, \(.) .[1, ]) |>
  tibble::enframe(name = "clean_address") |>
  tidyr::unnest(cols = c(value))
  
out <- left_join(select(d, -raw_address), all_matches, by = "clean_address", relationship = "many-to-many")

# view all addresses from 5 random study participants
out |>
  nest_by(MRN) |>
  ungroup() |>
  sample_n(5) |>
  tidyr::unnest(cols = c(data)) |>
  knitr::kable()
