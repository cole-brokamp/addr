devtools::load_all()
library(dplyr, warn.conflicts = FALSE)

input_data <-
  readr::read_csv(
    "../riseup_geomarker_pipeline/data/DR1767_r2.csv",
    na = c("NA", "-", "NULL", "null"),
    col_types = readr::cols_only(
      MRN = readr::col_character(),
      PAT_ENC_CSN_ID = readr::col_character(),
      ADDRESS = readr::col_character(),
      CITY = readr::col_character(),
      STATE = readr::col_character(),
      ZIP = readr::col_character()
    )
  ) |>
  tidyr::unite("raw_address", c(ADDRESS, CITY, STATE, ZIP), sep = " ", na.rm = TRUE) |>
  mutate(clean_address = clean_address_text(raw_address)) |>
  filter(!clean_address == "") |>
  mutate(addr = addr(clean_address))

# randomly select one address per patient in a subpopulation for this example
set.seed(1)
d <-
  input_data |>
  group_by(MRN) |>
  slice_sample(n = 1) |>
  ungroup() |>
  slice_sample(prop = 0.1)
nrow(d)

# use addr::cagis_addr reference addresses included with the package
d$cagis_addr_matches <- addr_match(d$addr, cagis_addr$addr)

## d <- filter(d, purrr::map_lgl(cagis_addr_matches, \(.) !is.null(.)))
## nrow(d)

# NULL addr_matches means the zip code wasn't matched at all in the reference set
# addr[0] means the zip code did match in the reference set, but no matches were found
d_no_match <- filter(d, purrr::map_lgl(cagis_addr_matches, vctrs::vec_is_empty))
d_single_match <- filter(d, purrr::map_lgl(cagis_addr_matches, \(.) vctrs::vec_size(.) == 1))
d_multi_match <- filter(d, purrr::map_lgl(cagis_addr_matches, \(.) vctrs::vec_size(.) > 1))


