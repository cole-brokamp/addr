#' Tag components of an address string
#' The address components are based upon the [United States Thoroughfare,
#' Landmark, and Postal Address Data Standard](http://www.urisa.org/advocacy/united-states-thoroughfare-landmark-and-postal-address-data-standard)
#' @param x a character vector of addresses
#' @param clean_address_text logical; clean addresses prior to tagging with `addr_clean()`?
#' @return a list, the same length as x, of named character vectors of address component tags;
#' each vector contains all space-separated elements of the cleaned address
#' and are each named based on inferred address labels (see Details)
#' @details
#' Possible address labels include:
#'
#'  - `AddressNumber`
#'  - `StreetNamePreDirectional`
#'  - `StreetName`
#'  - `StreetNamePostType`
#'  - `OccupancyIdentifier`
#'  - `OccupancyType`
#'  - `StreetNamePreType`
#'  - `PlaceName`
#'  - `ZipCode`
#'  - `StateName`
#'  - `LandmarkName`
#'  - `USPSBoxType`
#'  - `USPSBoxID`
#'  - `StreetNamePostDirectional`
#'  - `AddressNumberSuffix`
#'  - `USPSBoxGroupID`
#'  - `USPSBoxGroupType`
#'  - `SubaddressIdentifier`
#'  - `SubaddressType`
#'  - `Recipient`
#'  - `StreetNamePreModifier`
#'  - `BuildingName`
#'  - `AddressNumberPrefix`
#'  - `IntersectionSeparator`
#'  - `CornerOf`
#'  - `NotAddress`
#' @export
#' @examples
#' addr_tag(c("290 Ludlow Avenue Apt #2 Cincinnati OH 45220", "3333 Burnet Ave Cincinnati OH 45219"))
addr_tag <- function(x, clean_address_text = TRUE) {
  if (clean_address_text) x <- addr_clean(x)
  tags <-
    x |>
    usaddress_tag()
  lapply(tags, \(.) stats::setNames(names(.), .))
}
