#' matching addr vectors
#'
#' For an addr vector, the string distances are calculated between a reference addr vector (`ref_addr`).
#' A list of matching reference addr vectors less than or equal to the specified
#' [optimal string alignment](https://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance#Optimal_string_alignment_distance)
#' distances are returned.
#' See `stringdist::stringdist-metrics` for more details on string metrics and the optimal string alignment (`osa`) method.
#' @param x an addr vector to match
#' @param ref_addr an addr vector to search for matches in
#' @returns for `addr_match()` and `addr_match_street_name_number()`,
#' a named list of possible addr matches for each addr in `x`
#' @examples
#' addr(c("3333 Burnet Ave Cincinnati OH 45229", "5130 RAPID RUN RD CINCINNATI OHIO 45238")) |>
#'   addr_match(cagis_addr()$cagis_addr) |>
#'   tibble::enframe(name = "input_addr", value = "ca") |>
#'   dplyr::mutate(ca = purrr::list_c(ca)) |>
#'   dplyr::left_join(cagis_addr(), by = c("ca" = "cagis_addr"))
#' @export
addr_match <- function(x, ref_addr, stringdist_match = c("osa_lt_1", "exact"), match_street_type = TRUE) {
  ia <- stats::na.omit(unique(as_addr(x)))
  ra <- unique(as_addr(ref_addr))

  ia_zip_list <- split(ia, vctrs::field(ia, "zip_code"))

  ra_zip_list <- split(ra, vctrs::field(ra, "zip_code"))
  ra_zip_list <-
    names(ia_zip_list) |>
    purrr::map(\(.) purrr::pluck(ra_zip_list, ., .default = NA)) |>
    purrr::set_names(names(ia_zip_list))

  zip_list <-
    purrr::transpose(list(ia = ia_zip_list, ra = ra_zip_list)) |>
    purrr::discard(\(.) any(is.na(.$ia), is.na(.$ra)))

  matches <-
    purrr::map(zip_list, \(.) addr_match_street_name_and_number(.$ia, .$ra),
      .progress = list(
        clear = FALSE,
        format = "matching addresses in {cli::pb_current}/{cli::pb_total} ZIP codes [{cli::pb_elapsed} elapsed] "
      )
    ) |>
    purrr::list_flatten(name_spec = "{inner}")

  out <- matches[as.character(x)]
  names(out) <- as.character(x)
  return(out)
}

## addr_match_zip <- function(input_addr, ref_addr) {
##   zip_dist <- stringdist::stringdistmatrix(vctrs::field(input_addr, "zip_code"), vctrs::field(ref_addr, "zip_code"))
##   exact_matches <- apply(zip_dist, MARGIN = 1, FUN = \(.) which(. == 0), simplify = FALSE)
##   names(exact_matches) <- as.character(input_addr)
##   return(exact_matches)
## }

#' match addresses street names and numbers
#'
#' @param ... specific matching options passed on to `addr_match_street()`
#' @rdname addr_match
addr_match_street_name_and_number <- function(x, ref_addr, ...) {
  street_name_matches <-
    addr_match_street(x, ref_addr, ...)
  street_number_matches <-
    stringdist::stringdistmatrix(
      vctrs::field(x, "street_number"),
      vctrs::field(ref_addr, "street_number"),
    ) |>
    apply(MARGIN = 1, FUN = \(.) which(. <= 0), simplify = FALSE)
  the_matches <- purrr::map2(street_name_matches, street_number_matches, intersect)
  out <-
    purrr::map(the_matches, \(.) ref_addr[.]) |>
    purrr::set_names(x)
  return(out)
}


#' match addresses by street names
#'
#' @param stringdist_match method for determining string match of street name:
#' "osa_lt_1" requires an optimized string distance less than 1; "exact" requires an exact match
#' @param match_street_type logical; require street type to be identical to match?
#' @returns for addr_match_street, a list of possible addr matches for each addr in `x` (as `ref_addr` indices)
#' @rdname addr_match
addr_match_street <- function(x, ref_addr,
                              stringdist_match = c("osa_lt_1", "exact"),
                              match_street_type = TRUE) {
  stringdist_match <- rlang::arg_match(stringdist_match)

  street_name_dist <-
    stringdist::stringdistmatrix(vctrs::field(x, "street_name"), vctrs::field(ref_addr, "street_name"))

  exact_matches <- apply(street_name_dist, MARGIN = 1, FUN = \(.) which(. == 0), simplify = FALSE)

  if (stringdist_match == "exact") {
    the_matches <- exact_matches
  } else if (stringdist_match == "osa_lt_1") {
    one_off_matches <- apply(street_name_dist, MARGIN = 1, FUN = \(.) which(. == 1), simplify = FALSE)
    the_matches <- ifelse(lapply(exact_matches, length) != 0, exact_matches, one_off_matches)
  }

  if (match_street_type) {
    street_type_matches <-
      stringdist::stringdistmatrix(vctrs::field(x, "street_type"), vctrs::field(ref_addr, "street_type")) |>
      apply(MARGIN = 1, FUN = \(.) which(. == 0), simplify = FALSE)
    the_matches <- purrr::map2(the_matches, street_type_matches, intersect)
  }
  return(the_matches)
}


utils::globalVariables(c("ia_zips", "ra_zips"))
