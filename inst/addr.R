library(vctrs)
library(zeallot)

new_addr <- function(street_number = integer(),
                     street_name = character(),
                     street_type = character(),
                     city = character(),
                     state = character(),
                     zip_code = character()) {
  if (!rlang::is_integer(street_number)) rlang::abort("`street_number` must be an integer vector.")
  if (!rlang::is_character(street_name)) rlang::abort("`street_name` must be a character vector.")
  if (!rlang::is_character(street_type)) rlang::abort("`street_type` must be a character vector.")
  if (!rlang::is_character(city)) rlang::abort("`city` must be a character vector.")
  if (!rlang::is_character(state)) rlang::abort("`state` must be a character vector.")
  if (!rlang::is_character(zip_code)) rlang::abort("`zip_code` must be a character vector.")
  if (!all(grepl("^[[:digit:]]+$", zip_code))) rlang::abort("`zip_code` must not contain non-digit characters.")
  if (any(nchar(zip_code) > 5)) {
    warning("Truncating some ZIP Codes with more than 5 characters.")
  }
  c(street_number, street_name, street_type, city, state, zip_code) %<-%
    vec_recycle_common(street_number, street_name, street_type, city, state, zip_code)
  new_rcrd(
    list(
      street_number = street_number,
      street_name = tolower(street_name),
      street_type = tolower(street_type),
      city = tolower(city),
      state = tolower(state),
      zip_code = zip_code
    ),
    class = "addr"
  )
}

x <- new_addr(street_number = as.integer(c(3333, 1324)), street_name = "Burnet Ave", city = "Cincinnati", state = "OH", zip_code = "45219")
vec_data(x)

vec_cast.addr.character <- function(x, to, ...) {
  with(vec_data(x), {
    paste(
      street_number,
      stringr::str_to_title(street_name),
      stringr::str_to_title(city),
      stringr::str_to_upper(state),
      zip_code
    )
  })
  # TODO deal with missing components here?
}

vec_ptype_abbr.addr <- function(x, ...) "addr"
vec_ptype_full.addr <- function(x, ...) "addr"

format.addr <- function(x, ...) {
  vec_cast.addr.character(x)
  ## vec_cast(x, character())
}
x

fields(x)
field(x, "zip_code")
field(x, "street_number")
vec_data(x)

x <- new_addr(street_number = as.integer(c(3333, 1324)), street_name = "Burnet Ave", city = "Cincinnati", state = "OH", zip_code = "45219")

x <- c("3333 Burnet Ave Cincinnati OH 45219", "1324 Burnet Ave Cincinnati OH 45219")

#' @param x a character vector of address strings
#' @param clean_address_text logical; use `clean_address_text()` to clean addresses prior to tagging?
#' @param expand_street_name_post_type logical; use `expand_post_type()` to expand `StreetNamePostType` tags?
#' @param clean_zip_code logical; truncate tagged ZIP Code to 5 characters and set to missing if any of them are not digits?
#' @examples
#' addr(c("3333 Burnet Ave Cincinnati OH 45219", "1324 Burnet Ave Cincinnati OH 45219"))
addr <- function(x = character(),
                 clean_address_text = TRUE,
                 expand_street_type = TRUE,
                 clean_zip_code = TRUE) {
  x_tags <- addr_tag(vec_cast(x, character()), clean_address_text = clean_address_text)
  safe_extract_one <- function(x, name) {
    out <- paste(x[names(x) == name], collapse = " ")
    if (out == "") out <- NA
    return(out)
  }
  toi_names <- c("AddressNumber", "StreetName", "StreetNamePostType", "PlaceName", "StateName", "ZipCode")
  toi <- 
    purrr::map(toi_names, \(.) purrr::map_chr(x_tags, safe_extract_one, .)) |>
      setNames(toi_names)
  if (expand_street_type) toi$StreetNamePostType <- expand_post_type(toi$StreetNamePostType)
  if (clean_zip_code && "ZipCode" %in% toi_names) {
    toi$ZipCode <- substr(toi$ZipCode, 1, 5)
  }
  new_addr(
    street_number = vec_cast(as.numeric(toi$AddressNumber), integer()),
    street_name = vec_cast(toi$StreetName, character()),
    street_type = vec_cast(toi$StreetNamePostType, character()),
    city = vec_cast(toi$PlaceName, character()),
    state = vec_cast(toi$StateName, character()),
    zip_code = vec_cast(toi$ZipCode, character())
  )
}
