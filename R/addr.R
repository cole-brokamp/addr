#' Create a new addr object
#'
#' An addr object is created by converting messy, real-world mailing addresses in a
#' character vector into standardized address tags for comparison and lookup with `addr_tag()`.
#' By default, address strings are cleaned with `addr_clean()`,
#' ZIP codes are restricted to the first five digits and set to missing
#' if they contain any non-digit characters,
#' and the street name post types are expanded (e.g., "Ave" -> "Avenue").
#' @details
#' In the case of an address having more than one word for a tag (e.g., "Riva Ridge" for `StreetName`),
#' then these are concatenated together, separated by a space in the order they appeared in the address.
#' @param x a character vector of address strings
#' @param clean_address_text logical; use `clean_address_text()` to clean address text prior to tagging?
#' @param expand_street_type logical; use `expand_post_type()` to expand `StreetNamePostType` tags?
#' @param clean_zip_code logical; remove any non-digit (or hyphen) characters and truncate tagged ZIP Code to 5 characters?
#' @export
#' @examples
#' addr(c("3333 Burnet Ave Cincinnati OH 45229", "1324 Burnet Ave Cincinnati OH 45229"))
addr <- function(x = character(),
                 clean_address_text = TRUE,
                 expand_street_type = TRUE,
                 clean_zip_code = TRUE) {
  x_tags <- addr_tag(vec_cast(x, character()), clean_address_text = clean_address_text)
  safe_extract_one <- function(x, name) {
    out <- paste(x[names(x) %in% name], collapse = " ")
    if (out == "") out <- NA
    return(out)
  }
  toi_names <- list(
    "street_number" = c("AddressNumber"),
    "street_name" = c("StreetNamePreType", "StreetNamePreDirectional", "StreetName"),
    "street_type" = c("StreetNamePostType", "StreetNamePostDirectional"),
    "city" = c("PlaceName"),
    "state" = c("StateName"),
    "zip_code" = c("ZipCode")
  )
  toi <-
    purrr::map(toi_names, \(.) purrr::map_chr(x_tags, safe_extract_one, .)) |>
    stats::setNames(names(toi_names))
  if (expand_street_type) {
    toi$street_type <- expand_post_type(toi$street_type)
  }
  if (clean_zip_code && "zip_code" %in% names(toi_names)) {
    toi$zip_code <- gsub("[^0-9-]", "", toi$zip_code)
    toi$zip_code <- substr(toi$zip_code, 1, 5)
  }
  new_addr(
    street_number = vec_cast(as.numeric(toi$street_number), numeric()),
    street_name = vec_cast(toi$street_name, character()),
    street_type = vec_cast(toi$street_type, character()),
    city = vec_cast(toi$city, character()),
    state = vec_cast(toi$state, character()),
    zip_code = vec_cast(toi$zip_code, character())
  )
}

new_addr <- function(street_number = numeric(),
                     street_name = character(),
                     street_type = character(),
                     city = character(),
                     state = character(),
                     zip_code = character()) {
  if (!rlang::is_double(street_number)) rlang::abort("`street_number` must be a numeric vector.")
  if (!rlang::is_character(street_name)) rlang::abort("`street_name` must be a character vector.")
  if (!rlang::is_character(street_type)) rlang::abort("`street_type` must be a character vector.")
  if (!rlang::is_character(city)) rlang::abort("`city` must be a character vector.")
  if (!rlang::is_character(state)) rlang::abort("`state` must be a character vector.")
  if (!rlang::is_character(zip_code)) rlang::abort("`zip_code` must be a character vector.")
  if (any(grepl("[^0-9-]", zip_code))) rlang::abort("`zip_code` must contain only digit (or hyphen) characters.")
  if (any(!is.na(zip_code) & nchar(zip_code) > 5)) rlang::abort("`zip_code` must not be longer than five characters")
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

#' @importFrom methods setOldClass
# for compatibility with the S4 system
methods::setOldClass(c("addr", "vctrs_vctr"))

#' @export
vec_cast.addr.character <- function(x, to, ...) {
  xd <- vec_data(x)
  out <- paste(
    xd$street_number,
    stringr::str_to_title(xd$street_name),
    stringr::str_to_title(xd$street_type),
    stringr::str_to_title(xd$city),
    stringr::str_to_upper(xd$state),
    xd$zip_code
  )
  gsub(" NA", "", out, fixed = TRUE)
}

#' @export
format.addr <- function(x, ...) {
  vec_cast.addr.character(x)
}

#' @export
as.data.frame.addr <- function(x, ...) {
  vctrs::vec_data(x)
}

#' Coerce a character vector to an addr vector
#'
#' Compared to using `addr()`, `as_addr()` processes input character strings such that
#' parsing is done once per unique input, usually speeding up address parsing in real-world
#' datasets where address strings are often duplicated across observations.
#' @param x a character vector or addr vector
#' @param ... arguments passed onto `addr()`
#' @return an addr vector
#' @export
as_addr <- function(x, ...) {
  if (inherits(x, "addr")) {
    return(x)
  }
  ux <- unique(x)
  u_out <- as.list(addr(ux, ...))
  names(u_out) <- ux
  purrr::list_c(u_out[x])
}

#' @export
vec_ptype_abbr.addr <- function(x, ...) "addr"

#' @export
vec_ptype_full.addr <- function(x, ...) "addr"
