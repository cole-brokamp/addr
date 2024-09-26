#' geocode addr vectors
#'
#' Addresses are matched to a set of reference addresses; if unmatched,
#' they are next matched to TIGER street ranges by number and street name, returning the centroid
#' of the (unionized) matched street range(s).
#' @param x an addr vector to geocode
#' @param ref_addr an addr vector to search for matches in
#' @param ref_s2 a s2_cell vector of locations for each ref_addr
#' @param county character county identifer for TIGER street range files to search for matches in
#' @param year character year for TIGER street range files to search for matches in
#' @returns a s2_cell vector
#' @export
#' @details
#'
#' Performance was compared to the degauss geocoder (see `/inst/compare_geocoding_to_degauss.R`) using
#' real-world addresses in `voter_addresses()`.
#' Match success rates were highly similar:
#'
#' |addr_matched |degauss_matched |      n|perc  |
#' |:------------|:---------------|------:|:-----|
#' |TRUE         |TRUE            | 224974|92.9% |
#' |FALSE        |TRUE            |  13713|5.7%  |
#' |FALSE        |FALSE           |   2687|1.1%  |
#' |TRUE         |FALSE           |    759|0.3%  |
#'
#' Among those that were geocoded by both, 97.7% were geocoded to the same census tract:
#'
#' |ct_agree |bg_agree |      n|s2_dist_ptiles (5th, 25th, 50th, 75th, 95th) |perc  |
#' |:--------|:--------|------:|:--------------------------------------------|:-----|
#' |TRUE     |TRUE     | 217176|14.7, 24.3, 39, 68.9, 153.6                  |96.5% |
#' |FALSE    |FALSE    |   5067|21.9, 40.8, 212.5, 6616.5, 20222.2           |2.3%  |
#' |TRUE     |FALSE    |   2731|19.6, 28.6, 41.2, 95.1, 569.8                |1.2%  |
#' @examples
#' cagis_s2 <-
#'   cagis_addr()$cagis_addr_data |>
#'   purrr::modify_if(\(.) length(.) > 0 && nrow(.) > 1, dplyr::slice_sample, n = 1) |>
#'   purrr::map_vec(purrr::pluck, "cagis_s2", .default = NA, .ptype = s2::s2_cell())
#' addr_match_geocode(x = sample(voter_addresses(), 100), ref_s2 = cagis_s2)
addr_match_geocode <- function(x,
                               ref_addr = cagis_addr()$cagis_addr,
                               ref_s2,
                               county = "39061",
                               year = "2022") {
  x_addr <- as_addr(x)
  x_addr_ref_match <-
    addr_match(x_addr,
      ref_addr,
      stringdist_match = "osa_lt_1",
      match_street_type = TRUE,
      simplify = TRUE
    )

  x_addr_ref_match_which <- match(x_addr_ref_match, ref_addr)

  x_s2 <-
    ref_s2[x_addr_ref_match_which] |>
    stats::setNames(x_addr)

  x_addr_ref_no_match_which <- is.na(x_s2)

  t_matches <-
    addr_match_tiger_street_ranges(
      x_addr[x_addr_ref_no_match_which],
      county = county,
      year = year,
      summarize = "centroid",
    ) |>
    purrr::discard(\(.) length(.) < 1) |> # removes NULL
    purrr::discard(\(.) nrow(.) < 1) |> # removes empty data.frame
    purrr::map_vec(\(.) s2::as_s2_cell(.$s2_geography), .ptype = s2::s2_cell())

  x_which_addr_tiger_match <- match(names(t_matches), names(x_s2))
  x_s2[x_which_addr_tiger_match] <- t_matches

  x_mm <- rep(NA, length = length(x_s2))
  x_mm[!is.na(x_addr_ref_match_which)] <- "ref_addr"
  x_mm[x_which_addr_tiger_match] <- "tiger_range"
  x_mm[is.na(x_mm)] <- "none"

  return(tibble::tibble(addr = x_addr, s2 = x_s2, match_method = x_mm))
}
