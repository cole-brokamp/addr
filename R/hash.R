#' Address hashing (hashdress)
#'
#' Addresses are standardized (`addr_standardize()`) and then hashed using the md5 algorithm
#' via `digest::hash()`
#' @param x a character vector of address strings
#' @return a character vector of hashdresses
#' @export
#' @examples
#' addr_hash(c(
#'   "224 Woolper Avenue Apt 2 Cincinnati OH 45220",
#'   "224 wOoLpEr Avenue Cincinnati OH 45220",
#'   "224 Woolper Avenue Apt #2 Cincinnati OH 45220",
#'   "224 Woolper Ave Cincinnati OH 45220",
#'   "224 Woolper Av. Cincinnati OH 45220"
#' ))
addr_hash <- function(x) {
  rlang::check_installed("digest", "to create hashdresses.")
  x_std <- addr_standardize(x)
  x_hash <- vapply(x_std, \(.) digest::digest(., algo = "md5", serialize = FALSE), character(1), USE.NAMES = FALSE)
  # NA_character_ is still hashed; change these back to NA
  x_hash[x_hash == digest::digest(NA_character_, algo = "md5", serialize = FALSE)] <- NA_character_
  return(x_hash)
}
