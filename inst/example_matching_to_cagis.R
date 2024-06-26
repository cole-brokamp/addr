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

# add matching result
d <- d |>
  mutate(
    addr_match_result =
      case_when(
        purrr::map_lgl(cagis_addr_matches, is.null) ~ NA,
        purrr::map_dbl(cagis_addr_matches, vctrs::vec_size) == 0 ~ "no_match",
        purrr::map_dbl(cagis_addr_matches, vctrs::vec_size) == 1 ~ "match",
        purrr::map_dbl(cagis_addr_matches, \(.) length(unique(.))) == 1 ~ "multi_match_identical",
        purrr::map_dbl(cagis_addr_matches, vctrs::vec_size) > 1 ~ "multi_match",
        .default = "foofy"
      )
  )

table(d$addr_match_result, useNA = "always")

## street names shorter than 4 characters should not be considered a match unless oas distance == 0
## (e.g., "117 12th Street" and "117 13th Street")

## a high number of multimatches are likely condos?

## are there duplicated cagis addresses? check address guids are distinct?
