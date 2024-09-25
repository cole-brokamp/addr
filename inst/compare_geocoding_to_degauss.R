devtools::load_all()
library(dplyr, warn.conflicts = FALSE)
library(s2)

sessionInfo()

addr_s2 <-
  readRDS("inst/voter_geocode_addr.rds") |>
  rename(addr_s2 = s2) |>
  select(-matched_cagis_addr)

#### I used https://github.com/degauss-org/geocoder.sif, but something like:
## ia_d <- tibble::tibble(address = voter_addresses())
## readr::write_csv(ia_d, "inst/addr-v0.3.0_voter_addresses.csv")

## system2(
##   "docker",
##   c(
##     "run", "--rm",
##     "-v ${PWD}/inst:/tmp",
##     "ghcr.io/degauss-org/geocoder:3.3.0-v8",
##     "addr-v0.3.0_voter_addresses.csv"
##   )
## )

degauss_s2 <-
  readr::read_csv(
    "inst/addr-v0.3.0_voter_addresses_geocoder_3.3.0_score_threshold_0.5.csv",
    col_types = readr::cols_only(
      address = readr::col_character(),
      lat = readr::col_double(),
      lon = readr::col_double(),
    )
  ) |>
  mutate(
    addr = as_addr(address),
    degauss_s2 = as_s2_cell(s2_lnglat(lon, lat)),
    .keep = "unused"
  )

# doing this instead of a left_join because when some addr objects with missing address numbers
# are converted to character, they are no longer unique
d <- dplyr::bind_cols(arrange(addr_s2, addr), degauss_s2 = arrange(degauss_s2, addr)$degauss_s2)

d |>
  mutate(
    addr_bg = tiger_block_groups(addr_s2, year = "2020"),
    degauss_bg = tiger_block_groups(degauss_s2, year = "2020")
  )
