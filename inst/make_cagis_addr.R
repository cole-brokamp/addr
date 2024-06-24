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

# read in cagis addresses:
# - use only addresses that have `STATUS` of `ASSIGNED` or `USING` and are not orphaned (`ORPHANFLG == "N"`)
# - omit addresses with `ADDRTYPE`s that are milemarkers (`MM`), parks (`PAR`), infrastructure projects (`PRJ`),
#   cell towers (`CTW`), vacant or commercial lots (`LOT`), and other miscellaneous non-residential addresses (`MIS`, `RR`, `TBA`)
# - s2 cell derived from LONGITUDE and LATITUDE fields in CAGIS address database
ca <-
  install_cagis_data("https://www.cagis.org/Opendata/Quarterly_GIS_Data/CAGISOpenDataSpring2024.gdb.zip") |>
  sf::st_read(layer = "Addresses") |>
  sf::st_drop_geometry() |>
  tibble::as_tibble() |>
  filter(STATUS %in% c("ASSIGNED", "USING")) |>
  filter(ORPHANFLG == "N") |>
  filter(!ADDRTYPE %in% c("MM", "PAR", "PRJ", "CTW", "LOT", "MIS", "RR", "TBA")) |>
  # TODO should these be called cagis_address, etc so output is clearer?
  transmute(
    address = FULLMAILADR,
    address_place = BLDGPLACE,
    address_type = ADDRTYPE,
    s2 = s2::as_s2_cell(s2::s2_geog_point(LONGITUDE, LATITUDE)),
    parcel_id = PARCELID,
    is_condo = CONDOFLG %in% c("Y")
  )

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

# TODO include street_prefix? (missing things like State Rt for street names...)

#' @param input_addr
#' @param ref_addr
#' @return list of integer vectors, representing the indices of matched address in `ref_addr` for each address in `input_addr`
#' @details To increase efficiency, input_addr are reduced to unique values for matching
addr_match <- function(input_addr, ref_addr) {
  # TODO instead of below, create and use is_addr and as_addr functions to optionally accept char vectors to run through addr() first
  ## c(ia, ra) %<-% lapply(list(input_addr, ref_addr), as_addr)
  ia <- addr(d$clean_address)
  ra <- addr(ca$address)

  ia <- unique(ia)

  c(ia_zips, ra_zips) %<-% lapply(list(ia, ra), \(.) unique(vctrs::field(., "zip_code")))
  na_ra_zips <- ia_zips[which(!ia_zips %in% ra_zips)]
  if (length(na_ra_zips) != 0) {
    rlang::warn(paste(
      "Not attempting to match",
      prettyNum(length(na_ra_zips), ","), "of",
      prettyNum(length(ia_zips), ","), "unique input_addr zip codes not available in",
      prettyNum(length(ra_zips), ","), "unique ref_addr zip codes."
    ))
    ia <- ia[vctrs::field(ia, "zip_code") %in% ra_zips]
  }

  ia_z <- split(ia, vctrs::field(ia, "zip_code"))
  ra_z <- split(ra, vctrs::field(ra, "zip_code"))
  all_matches_by_zip <-
    vector("list", length = length(ia_z)) |>
    setNames(names(ia_z))
  message(
    "Processing unique address observations in ",
    length(ia_z),
    " ZIP Code groupings."
  )
  tictoc::tic("Finished processing all ZIP codes")
  for (zc in names(ia_z)) {
    tictoc::tic(paste(
      "[[", zc, "]]: matching",
      prettyNum(length(ia_z[[zc]]), ","), "unique input addresses to",
      prettyNum(length(ra_z[[zc]]), ","), "total reference addresses"
    ))
    street_matches <-
      stringdist::stringdistmatrix(
        vctrs::field(ia_z[[zc]], "street_name"),
        vctrs::field(ra_z[[zc]], "street_name")
      ) |>
      apply(MARGIN = 1, FUN = \(.) which(. <= 1))
    number_matches <-
      stringdist::stringdistmatrix(
        vctrs::field(ia_z[[zc]], "street_number"),
        vctrs::field(ra_z[[zc]], "street_number"),
      ) |>
      apply(MARGIN = 1, FUN = \(.) which(. <= 0))
    if (length(street_matches) == 0 | length(number_matches) == 0) {
      all_matches_by_zip[[zc]] <- list(rep.int(0, times = length(all_matches_by_zip[[zc]])))
    } else {
      all_matches_by_zip[[zc]] <-
        purrr::map2(street_matches, number_matches, intersect)
    }
    tictoc::toc()
  }
  tictoc::toc()

  out <-
    purrr::list_flatten(all_matches_by_zip, name_spec = "{inner}") |>
    setNames(vec_cast.addr.character(ia))
  out[[vec_cast.addr.character(input_addr)]]
}

matches <- addr_match(input_addr = addr(d$clean_address), ref_addr = addr(ca$address))

# use matched indices to extract matched data and addr based on row in ca
