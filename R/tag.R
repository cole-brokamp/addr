code <- r"(
fn usaddress_tag(input: Vec<String>) -> Robj {
    let ta: Vec<_> = input
	.iter().map(|x| usaddress::parse(x).unwrap())
	.map(|x| Pairlist::from_pairs(x))
	.collect();
    return(r!(List::from_values(ta)))
}
)"

rextendr::rust_function(
  code = code,
  dependencies = list(`usaddress` = "0.1")
  ## https://docs.rs/crate/usaddress/latest
)

# The address components are based upon the `United States Thoroughfare,
# Landmark, and Postal Address Data Standard
# http://www.urisa.org/advocacy/united-states-thoroughfare-landmark-and-postal-address-data-standard
#' @export
#' @examples
#' addr_tag(c("224 Woolper Avenue Apt #2 Cincinnati OH 45220", "3333 Burnet Ave Cincinnati OH 45219"))
addr_tag <- function(x) {
  tags <-
    x |>
    addr_clean() |>
    usaddress_tag()
  lapply(tags, \(.) stats::setNames(names(.), .))
}

## labels <- c(
##   "AddressNumber",
##   "StreetNamePreDirectional",
##   "StreetName",
##   "StreetNamePostType",
##   "OccupancyIdentifier",
##   "OccupancyType",
##   "StreetNamePreType",
##   "PlaceName",
##   "ZipCode",
##   "StateName",
##   "LandmarkName",
##   "USPSBoxType",
##   "USPSBoxID",
##   "StreetNamePostDirectional",
##   "AddressNumberSuffix",
##   "USPSBoxGroupID",
##   "USPSBoxGroupType",
##   "SubaddressIdentifier",
##   "SubaddressType",
##   "Recipient",
##   "StreetNamePreModifier",
##   "BuildingName",
##   "AddressNumberPrefix",
##   "IntersectionSeparator",
##   "CornerOf",
##   "NotAddress"
## )
