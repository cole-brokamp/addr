#' Address standardization
#'
#' Convert messy, real-world mailing addresses into standardized address tags for comparison and lookup. 
#' By default, address strings are cleaned with `addr_clean()`, the ZIP codes are restricted to the first
#' five digits, and the street name post types are expanded (e.g., "Ave" -> "Avenue").
#' In the case of an address having more than one word for a tag (e.g., "Riva Ridge" for `StreetName`),
#' then these are concatenated together, separated by a space in the order they appeared in the address.
#' @param x a character vector of address strings
#' @param tags a character vector of tag names to be used, in order, to create the standardized address
#' (see `addr_tag()` for all possibilities)
#' @param clean_address_text logical; use `clean_address_text()` to clean addresses prior to tagging?
#' @param expand_street_name_post_type logical; use `expand_post_type()` to expand `StreetNamePostType` tags?
#' @param five_digit_zip logical; return only the first five digits of the parsed `ZipCode` tag?
#' @return a list the same length as x, where each item is a character vector of address tags specified in `tags`
#' @export
#' @examples
#' addr_standardize(
#'   x = c(
#'     "3333 Burnet Avenue Apt 2 Cincinnati OH 45220",
#'     "3333 bUrNeT Avenue Cincinnati OH 45220",
#'     "3333 Burnet Avenue Apt #2 Cincinnati OH 45220",
#'     "3333 Burnet Ave Cincinnati OH 45220",
#'     "3333 Burnet Av. Cincinnati OH 45220",
#'     "3333 Burnet Avenue Cincinnati 45220"
#'   )
#' )
addr_standardize <- function(x,
                             tags = c("AddressNumber", "StreetName", "StreetNamePostType", "PlaceName", "StateName", "ZipCode"),
                             clean_address_text = TRUE,
                             expand_street_name_post_type = TRUE,
                             five_digit_zip = TRUE) {
  x_tags <- addr_tag(x, clean_address_text = clean_address_text)
  tags <- rlang::arg_match(tags, multiple = TRUE)

  safe_extract_one <- function(x, name) {
    out <- paste(x[names(x) == name], collapse = " ")
    if (out == "") out <- NA
    return(out)
  }

  standard_tags <-
    purrr::map(tags, \(.) purrr::map_chr(x_tags, safe_extract_one, .)) |>
    stats::setNames(tags)

  if (five_digit_zip && "ZipCode" %in% tags) standard_tags$ZipCode <- substr(standard_tags$ZipCode, 1, 5)

  if (expand_street_name_post_type) standard_tags$StreetNamePostType <- expand_post_type(tolower(standard_tags$StreetNamePostType))

  out <- purrr::transpose(standard_tags)

  return(out)
}

#' Expand street name post type
#'
#' Abbreviations of street type (e.g., "Ave", "St") are converted to
#' expanded versions (e.g., "Avenue", "Street").
#' @param x character vector of `StreetnamePostType` abbreviations
#' @return a character vector of the same length containing the expanded street name post type
#' @export
#' @examples
#' expand_post_type(c("ave", "av", "Avenue", "tl"))
expand_post_type <- function(x) {
  lookup <-
    list(
      "Avenue" = c("av", "ave", "avnue"),
      "Boulevard" = c("blvd", "blvrd", "bv"),
      "Circle" = c("cir", "cr", "crcl"),
      "Court" = c("ct"),
      "Crescent" = c("cres"),
      "Drive" = c("dr", "drv"),
      "Highway" = c("hgwy", "hw", "hway", "hwy", "hywy"),
      "Lane" = c("ln"),
      ## "Path" = c("path"),
      "Parkway" = c("pkwy"),
      "Pike" = c("pk"),
      # 2406 South Rd, 2410 South Rd, 2268 South Rd
      # 3624 Westwood Northern Bv
      # 5105 State Route 128
      # 214 FOURTEENTH ST ST
      # 3040 SOUTH RD, 3058 SOUTH RD
      # 1312 BROADWAY
      "Place" = c("pl"),
      "Point" = c("pointe", "pt"), # palisades pointe
      "Road" = c("rd"),
      "Route" = c("rt"),
      "Street" = c("st", "str"),
      "Terrace" = c("te", "ter", "terr", "trce"),
      "Trail" = c("tr", "tl"),
      "Way" = c("wy")
    ) |>
    purrr::imap(\(.x, .i) stats::setNames(c(tolower(.i), as.character(.x)), rep(.i, times = length(.x) + 1))) |>
    purrr::flatten()
  out <- names(lookup[match(tolower(x), lookup)])
  # replace unmatched with original input
  out[is.na(out)] <- x[is.na(out)]
  return(out)
}
