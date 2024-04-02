#' Address standardization
#'
#' After cleaning and tagging addresses, specific tags are
#' concatenated to create a harmonized address suitable
#' for comparison and/or hashing.
#' Specific tags are separate by spaces; i.e., `{AddressNumber} {StreetName} {StreetNamePostType}
#' {PlaceName} {StateName} {ZipCode}`. All address tags are converted to lower case,
#' street name post types are expanded (e.g., "str" to "street" and "ave" to "avenue"),
#' and only the first five digits of the ZIP Code are used.
#' If any address tags are missing (except for `StreetNamePostType`), then a missing
#' standardized address will be returned.
#' @param x a character vector of address strings
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
addr_standardize <- function(x) {
  x_tags <- addr_tag(x)
  safe_glue_data <- purrr::safely(glue::glue_data)
  glues <-
    purrr::map(x_tags, \(i) {
      safe_glue_data(i,
                     "{AddressNumber}",
                     "{StreetName}",
                     "{expand_post_type(tolower(StreetNamePostType))}",
                     "{PlaceName}",
                     "{StateName}",
                     "{substr(ZipCode, 1, 5)}",
                     .sep = " ")
    })
  purrr::map_chr(glues, \(x) purrr::pluck(x, "result", .default = NA)) |>
    tolower()
}

#' Expand street name post type
#'
#' Abbreviations of street type (e.g., "Ave", "St") are converted to
#' expanded versions (e.g., "avenue", "street").
#' @param x character vector of length one
#' @return a character vector of length one representing the expanded street name post type
#' @export
#' @examples
#' expand_post_type("ave")
#' expand_post_type("av")
#' expand_post_type("av")
#' expand_post_type("avenue")
#' expand_post_type("st")
#' expand_post_type(NULL)
expand_post_type <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
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
      "terrace" = c("te", "ter", "terr"),
      "way" = c("wy")
    ) |>
    unlist()
  out <-
    lookup[match(tolower(x), lookup)] |>
    names() |>
    stringr::str_replace_all("[^a-z]", "")
  if (is.na(out)) {
    return(x)
  }
  return(out)
}

