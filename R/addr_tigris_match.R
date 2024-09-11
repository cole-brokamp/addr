#' Get tigris street range geography files from census.gov
#'
#' Downloaded files are cached in `tools::R_user_dir("addr", "cache")`.
#' @param county character string of county identifier
#' @param year year of tigris product
#' @returns a tibble of street names with a nested column of address range geographies with
#' min number, max number, id, and geography columns
#' @export
#' @examples
#' Sys.setenv("R_USER_CACHE_DIR" = tempfile())
#' d <- get_tigris_street_ranges("39061")
#' head(d)
#' d[3, "sf_tbl", drop = TRUE]
get_tigris_street_ranges <- function(county, year = "2022") {
  stopifnot(year == "2022")
  dl_url <- glue::glue("https://www2.census.gov/geo/tiger/TIGER2022/ADDRFEAT/tl_2022_{county}_addrfeat.zip")
  dest_path <- fs::path(tools::R_user_dir("addr", "cache"), glue::glue("tl_2022_{county}_addrfeat.zip"))
  fs::dir_create(fs::path_dir(dest_path))
  if (!fs::file_exists(dest_path)) {
    utils::download.file(dl_url, dest_path)
  }
  sf::st_read(
    dsn = paste0("/vsizip/", dest_path),
    query = "SELECT TLID, FULLNAME, LFROMHN, LTOHN, RFROMHN, RTOHN FROM tl_2022_39061_addrfeat",
    quiet = TRUE, stringsAsFactors = FALSE, as_tibble = TRUE
  ) |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("HN"), as.numeric),
      TLID = as.character(TLID)
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      from = min(LFROMHN, LTOHN, RFROMHN, RTOHN, na.rm = TRUE),
      to = max(LFROMHN, LTOHN, RFROMHN, RTOHN, na.rm = TRUE),
      .keep = "unused"
    ) |>
    dplyr::filter(from < Inf & to > -Inf) |>
    suppressWarnings() |>
    dplyr::ungroup() |>
    dplyr::nest_by(FULLNAME, .key = "sf_tbl") |>
    dplyr::ungroup()
}

#' match an addr vector to a tigris street range
#' @param x an addr vector to match
#' @param county character string of county identifier
#' @param year year of tigris product
#' @return a list of tigris street ranges matching the street name and containing the street number in x;
#' a NULL value indicates that no street name was matched; a street range tibble with zero rows indicates
#' that although a street was matched, there was no range containing the street number
#' @export
#' @examples
#' addr_match_tigris_street_ranges(as_addr(c("224 Woolper Ave", "3333 Burnet Ave",
#'                                           "33333 Burnet Ave", "609 Walnut St")))
addr_match_tigris_street_ranges <- function(x, county = "39061", year = "2022") {
  stopifnot(inherits(x, "addr"))
  ia <- unique(x)
  d_tiger <- get_tigris_street_ranges(county = county, year = year)

  street_matches <-
    addr_match_street(ia,
      suppressWarnings(as_addr(d_tiger$FULLNAME)),
      stringdist_match = "osa_lt_1",
      match_street_type = TRUE
    ) |>
    purrr::map(\(.) d_tiger[., "sf_tbl", drop = TRUE]) |>
    purrr::map(purrr::pluck, 1, .default = NA)

  no_match_street <- which(is.na(street_matches))
  ia[no_match_street] <- NA
  ia <- stats::na.omit(ia)
  street_matches[no_match_street] <- NULL
  stopifnot(length(ia) == length(street_matches))

  out <-
    purrr::map2(
      vctrs::field(ia, "street_number"), street_matches,
      \(.sn, .sm) dplyr::filter(.sm, from <= .sn, to >= .sn)
    ) |>
    stats::setNames(as.character(ia))

  return(stats::setNames(out[as.character(x)], as.character(x)))
}

utils::globalVariables(c("from", "to", "FULLNAME", "LFROMHN", "LTOHN", "RFROMHN", "RTOHN", "TLID"))
