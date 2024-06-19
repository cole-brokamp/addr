#' Address hashing (hashdress)
#'
#' @export
#' @examples
#' addr_hash(c(
#'   "224 Woolper Avenue Apt 2 Cincinnati OH 45220",
#'   "224 wOoLpEr Avenue Cincinnati OH 45220",
#'   "224 Woolper Avenue Apt #2 Cincinnati OH 45220",
#'   "224 Woolper Ave Cincinnati OH 45220",
#'   "224 Woolper Av. Cincinnati OH 45220"
#' ))
#' @rdname addr_standardize
addr_hash <- function(x,
                      tags = c("AddressNumber", "StreetName", "StreetNamePostType", "PlaceName", "StateName", "ZipCode"),
                      five_digit_zip = TRUE,
                      expand_street_name_post_type = TRUE,
                      clean_address_text = TRUE) {
  rlang::check_installed("digest", "to create hashdresses.")
  x_std <- addr_standardize(x, tags = tags)
  x_hash <- vapply(x_std, \(.) digest::digest(., algo = "md5", serialize = FALSE), character(1), USE.NAMES = FALSE)
  # NA_character_ is still hashed; change these back to NA
  x_hash[x_hash == digest::digest(NA_character_, algo = "md5", serialize = FALSE)] <- NA_character_
  return(x_hash)
}
