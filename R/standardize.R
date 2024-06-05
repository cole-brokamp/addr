#' Address standardization
#'
#' After cleaning and tagging addresses, specific tags are
#' concatenated (separated by spaces) to create a harmonized address suitable
#' for comparison and/or hashing.
#' Specific tags are separate by spaces; e.g., `{AddressNumber} {StreetName} {StreetNamePostType}
#' {PlaceName} {StateName} {ZipCode}`.
#' By default, all address tags are converted to lower case,
#' street name post types are expanded (e.g., "str" to "street" and "ave" to "avenue"),
#' and only the first five digits of the ZIP Code are used.
#' If any address tags are missing (except for `StreetNamePostType`), then a missing
#' standardized address will be returned.
#' In the case of an address having more than one word for a tag
#' (e.g., "Riva Ridge" for `StreetName`), then these are concatenated together, separated by a space
#' in the order they appeared in the address.
#' @param x a character vector of address strings
#' @param tags a character vector of tag names to be used, in order, to create the standardized address
#' @param five_digit_zip logical; return only the first five digits of the parsed ZIP code?
#' @param expand_street_name_post_type logical; use `expand_post_type()` to expand street type abbreviations
#' @param clean_address_text logical; clean addresses prior to tagging with `addr_clean()`?
#' @return a character vector of standardized address strings
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
                             five_digit_zip = TRUE,
                             expand_street_name_post_type = TRUE,
                             clean_address_text = TRUE) {
  x_tags <- addr_tag(x, clean_address_text = clean_address_text)

  safe_extract_one <- function(x, name) {
    out <- paste(x[names(x) == name], collapse = " ")
    if (out == "") out <- NA
    return(out)
  }

  standard_tags <-
    purrr::map(tags, \(.) purrr::map_chr(x_tags, safe_extract_one, .)) |>
    setNames(tags)

  if (five_digit_zip) standard_tags$ZipCode <- substr(standard_tags$ZipCode, 1, 5)

  if (expand_street_name_post_type) standard_tags$StreetNamePostType <- expand_post_type(tolower(standard_tags$StreetNamePostType))

  out <-
    purrr::transpose(standard_tags) |>
    purrr::map_chr(paste, collapse = " ") |>
    tolower()

  return(out)
}

#' Expand street name post type
#'
#' Abbreviations of street type (e.g., "Ave", "St") are converted to
#' expanded versions (e.g., "avenue", "street").
#' @param x character vector of `StreetnamePostType` abbreviations
#' @return a character vector of the same length containing the expanded street name post type
#' @export
#' @examples
#' expand_post_type(c("ave", "av", "Avenue"))
expand_post_type <- function(x) {
  lookup <-
    list(
      "avenue" = c("av", "ave", "avnue"),
      "boulevard" = c("blvd", "blvrd"),
      "circle" = c("cir", "cr", "crcl"),
      "court" = c("ct"),
      "drive" = c("dr", "drv"),
      "highway" = c("hgwy", "hw", "hway", "hwy", "hywy"),
      "lane" = c("ln"),
      "parkway" = c("pkwy"),
      "place" = c("pl"),
      "road" = c("rd"),
      "route" = c("rt"),
      "street" = c("st", "str"),
      "terrace" = c("te", "ter", "terr", "trce"),
      "way" = c("wy")
    ) |>
    purrr::imap(\(.x, .i) setNames(c(.i, as.character(.x)), rep(.i, times = length(.x) + 1))) |>
    purrr::flatten()
  return(names(lookup[match(tolower(x), lookup)]))
}
