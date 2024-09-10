#' Get tigris street range geography files from census.gov
#' @param county character string of county identifier
#' @param year year of tigris product
#' @returns a tibble of street names with a nested column of address range geographies with
#' min number, max number, id, and geography columns
#' @export
#' @example
#' get_tigris_street_ranges("39061")
get_tigris_street_ranges <- function(county, year = "2022") {
  stopifnot(year == "2022")
  tf <- tempfile(fileext = ".zip")
  glue::glue("https://www2.census.gov/geo/tiger/TIGER2022/ADDRFEAT/tl_2022_{county}_addrfeat.zip") |>
    download.file(destfile = tf)
  sf::st_read(
    dsn = paste0("/vsizip/", tf),
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
#' @examples
#' addr_match_tigris_street_ranges(as_addr(c("224 Woolper Ave", "3333 Burnet Ave", "609 Walnut St")))
addr_match_tigris_street_ranges <- function(x, county = "39061", year = "2022") {
  ia <- stats::na.omit(unique(as_addr(unique(x))))
  d_tiger <-
    get_tigris_street_ranges(county = county, year = year) |>
    dplyr::mutate(addr = as_addr(FULLNAME)) |>
    suppressWarnings()

  exact_street_matches <-
    stringdist::stringdistmatrix(
      vctrs::field(ia, "street_name"),
      vctrs::field(d_tiger$addr, "street_name")
    ) |>
    apply(MARGIN = 1, FUN = \(.) which(. == 0), simplify = FALSE)

  one_off_street_matches <-
    stringdist::stringdistmatrix(
      vctrs::field(ia, "street_name"),
      vctrs::field(d_tiger$addr, "street_name")
    ) |>
    apply(MARGIN = 1, FUN = \(.) which(. == 1), simplify = FALSE)

  street_type_matches <-
    stringdist::stringdistmatrix(
      vctrs::field(ia, "street_type"),
      vctrs::field(d_tiger$addr, "street_type")
    ) |>
    apply(MARGIN = 1, FUN = \(.) which(. <= 1), simplify = FALSE)

  if (length(exact_street_matches) == 0 | length(one_off_street_matches) == 0 | length(street_type_matches) == 0) {
    return(list(rep(addr(), times = length(input_addr))))
  }

  out <-
    purrr::map2(exact_street_matches, one_off_street_matches, \(.x, .y) {
      if (length(.x) > 0) {
        return(.x)
      } else {
        return(.y)
      }
    }) |>
    purrr::map2(street_type_matches, intersect) |>
    purrr::map(\(.x) d_tiger[.x, ]) |>
    purrr::map(\(.x) .x$sf_tbl[1][[1]]) |>
    setNames(as.character(ia))

  return(out[as.character(x)])
}
