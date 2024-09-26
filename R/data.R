#' CAGIS Addresses
#'
#' @returns An example tibble created from the CAGIS addresses with a pre-calculated, unique `cagis_addr` vector column.
#' The `cagis_addr_data` column is a list of tibbles because one CAGIS address can correspond to multiple
#' parcel identifiers and address-level data (place, type, s2, etc.).
#' See `inst/make_cagis_addr.R` for source code to create data, including filtering criteria:
#'
#' - use only addresses that have `STATUS` of `ASSIGNED` or `USING` and are not orphaned (`ORPHANFLG == "N"`)
#' - omit addresses with `ADDRTYPE`s that are milemarkers (`MM`), parks (`PAR`), infrastructure projects (`PRJ`),
#'   cell towers (`CTW`), vacant or commercial lots (`LOT`), and other miscellaneous non-residential addresses (`MIS`, `RR`, `TBA`)
#' - s2 cell is derived from LONGITUDE and LATITUDE fields in CAGIS address database
#' @export
#' @examples
#' cagis_addr()
cagis_addr <- function() {
  readRDS(fs::path_package("addr", "cagis_addr.rds"))
}

#' Example real-world addresses
#'
#' The voter_addresses data was generated as an example character vector of real-world addresses.
#' These addresses were downloaded from the Hamilton County, Ohio voter registration database on 2024-09-12.
#' See `inst/make_example_addresses.R` for more details.
#' `AddressPreDirectional`, `AddressNumber`, `AddressStreet`, `AddressSuffix`, `CityName`, "OH", and `AddressZip`
#' are pasted together to create 242,133 unique addresses of registered voters in Hamilton County, OH.
#' @returns a character vector
#' @export
#' @examples
#' voter_addresses() |>
#'   head()
voter_addresses <- function() {
  readRDS(fs::path_package("addr", "voter_addresses.rds"))
}
