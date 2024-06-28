#' CAGIS Addresses
#'
#' An example tibble created from the CAGIS addresses with a pre-calculated, unique `addr` vector column.
#' The `cagis_parcel_id` column is a list of character vectors because one CAGIS address can correspond to multiple
#' parcel identifiers.
#' See `data-raw/make_cagis_addr.R` for source code to create data, including filtering criteria:
#'
#' - use only addresses that have `STATUS` of `ASSIGNED` or `USING` and are not orphaned (`ORPHANFLG == "N"`)
#' - omit addresses with `ADDRTYPE`s that are milemarkers (`MM`), parks (`PAR`), infrastructure projects (`PRJ`),
#'   cell towers (`CTW`), vacant or commercial lots (`LOT`), and other miscellaneous non-residential addresses (`MIS`, `RR`, `TBA`)
#' - s2 cell is derived from LONGITUDE and LATITUDE fields in CAGIS address database
"cagis_addr"
