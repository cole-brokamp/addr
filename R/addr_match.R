#' matching addr vectors
#'
#' For an addr vector, the string distances are calculated between a reference addr vector (`ref_addr`).
#' A list of matching reference addr vectors less than or equal to the specified
#' [optimal string alignment](https://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance#Optimal_string_alignment_distance)
#' distances are returned.
#' See `stringdist::stringdist-metrics` for more details on string metrics and the optimal string alignment (`osa`) method.
#' @param x an addr vector to match
#' @param ref_addr an addr vector to search for matches in
#' @return a list of matching reference addr vectors for each addr in x
#' @examples
#' addr(c("3333 Burnet Ave Cincinnati OH 45229", "5130 RAPID RUN RD CINCINNATI OHIO 45238")) |>
#'   addr_match(cagis_addr()$cagis_addr) |>
#'   tibble::enframe(name = "input_addr", value = "ca") |>
#'   dplyr::mutate(ca = purrr::list_c(ca)) |>
#'   dplyr::left_join(cagis_addr(), by = c("ca" = "cagis_addr"))
#' @export
addr_match <- function(x, ref_addr) {
  ia <- stats::na.omit(unique(as_addr(unique(x))))
  ra <- as_addr(ref_addr)
  matched_zips <-
    addr_match_zip(ia, ra) |>
    unique() |>
    purrr::compact()
  all_matches <-
    matched_zips |>
    purrr::map(
      \(zc) addr_match_line_one(
        input_addr = ia[vctrs::field(ia, "zip_code") %in% zc],
        ref_addr = ra[vctrs::field(ra, "zip_code") %in% zc]
      ),
      .progress = list(
        clear = FALSE,
        format = "matching addresses in {cli::pb_current}/{cli::pb_total} ZIP codes [{cli::pb_elapsed} elapsed] "
      )
    ) |>
    purrr::list_flatten()
  all_matches[match(as.character(x), names(all_matches))]
}

# returns a list the same length as the input addresses where each item is a character vector of the unique matched zip codes in the reference addresses
addr_match_zip <- function(input_addr, ref_addr) {
  ia_zips <- unique(vctrs::field(input_addr, "zip_code"))
  ra_zips <- unique(vctrs::field(ref_addr, "zip_code"))
  match_lookup <-
    stringdist::stringdistmatrix(ia_zips, ra_zips, method = "osa") |>
    apply(MARGIN = 1, FUN = \(.) which(. <= 0)) |>
    stats::setNames(ia_zips)
  the_matches <- match_lookup[vctrs::field(input_addr, "zip_code")]
  lapply(the_matches, \(.) ra_zips[.]) |>
    stats::setNames(NULL)
}

# returns a list of possible addr matches in ref_addr for each addr in input_addr
# output is named by casting input_addr to character
addr_match_line_one <- function(input_addr, ref_addr) {
  exact_street_matches <-
    stringdist::stringdistmatrix(
      vctrs::field(input_addr, "street_name"),
      vctrs::field(ref_addr, "street_name")
    ) |>
    apply(MARGIN = 1, FUN = \(.) which(. == 0), simplify = FALSE)
  one_off_street_matches <-
    stringdist::stringdistmatrix(
      vctrs::field(input_addr, "street_name"),
      vctrs::field(ref_addr, "street_name")
    ) |>
    apply(MARGIN = 1, FUN = \(.) which(. == 1), simplify = FALSE)
  street_type_matches <-
    stringdist::stringdistmatrix(
      vctrs::field(input_addr, "street_type"),
      vctrs::field(ref_addr, "street_type")
    ) |>
    apply(MARGIN = 1, FUN = \(.) which(. <= 1), simplify = FALSE)
  number_matches <-
    stringdist::stringdistmatrix(
      vctrs::field(input_addr, "street_number"),
      vctrs::field(ref_addr, "street_number"),
    ) |>
    apply(MARGIN = 1, FUN = \(.) which(. <= 0), simplify = FALSE)
  if (length(exact_street_matches) == 0 | length(one_off_street_matches) == 0 | length(number_matches) == 0 | length(street_type_matches) == 0) {
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
    purrr::map2(number_matches, intersect) |>
    purrr::map2(street_type_matches, intersect) |>
    purrr::map(\(.) ref_addr[.]) |>
    stats::setNames(as.character(input_addr))
  return(out)
}

# returns a list of possible addr (street name and type) matches (by number) in ref_addr for each addr in input_addr
addr_match_street <- function(input_addr, ref_addr,
                              stringdist_match = c("osa_lt_1", "exact"),
                              match_street_type = TRUE) {
  sd_m <- rlang::arg_match(stringdist_match)

  street_name_dist <-
    stringdist::stringdistmatrix(vctrs::field(input_addr, "street_name"), vctrs::field(ref_addr, "street_name"))

  exact_matches <- apply(street_name_dist, MARGIN = 1, FUN = \(.) which(. == 0), simplify = FALSE)

  if (sd_m == "exact") {
    the_matches <- exact_matches
  } else if (sd_m == "osa_lt_1") {
    one_off_matches <- apply(street_name_dist, MARGIN = 1, FUN = \(.) which(. == 1), simplify = FALSE)
    the_matches <- ifelse(lapply(exact_matches, length) != 0, exact_matches, one_off_matches)
  }

  if (match_street_type) {
    street_type_matches <-
      stringdist::stringdistmatrix(vctrs::field(input_addr, "street_type"), vctrs::field(ref_addr, "street_type")) |>
      apply(MARGIN = 1, FUN = \(.) which(. == 0), simplify = FALSE)
    the_matches <- purrr::map2(the_matches, street_type_matches, intersect)
  }

  return(the_matches)
}

utils::globalVariables(c("ia_zips", "ra_zips"))
