#' CAGIS Addresses
#'
#' An example tibble created from the CAGIS addresses with an addr vector column. See `data-raw/make_cagis_addr.R` for details.
#' Addresses are filtered out based on some criteria:
#'
#' - use only addresses that have `STATUS` of `ASSIGNED` or `USING` and are not orphaned (`ORPHANFLG == "N"`)
#' - omit addresses with `ADDRTYPE`s that are milemarkers (`MM`), parks (`PAR`), infrastructure projects (`PRJ`),
#'   cell towers (`CTW`), vacant or commercial lots (`LOT`), and other miscellaneous non-residential addresses (`MIS`, `RR`, `TBA`)
#' - s2 cell derived from LONGITUDE and LATITUDE fields in CAGIS address database
"cagis_addr"
