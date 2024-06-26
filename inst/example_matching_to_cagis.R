devtools::load_all()
library(dplyr, warn.conflicts = FALSE)

d <-
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
  select(-raw_address) |>
  filter(!clean_address == "") |>
  mutate(addr = addr(clean_address))

# randomly select one address from 10% of all patients
d <- d |>
  group_by(MRN) |>
  slice_sample(n = 1) |>
  ungroup() |>
  slice_sample(prop = 0.1)

# use addr::cagis_addr reference addresses included with the package
d$cagis_addr_matches <- addr_match(d$addr, cagis_addr$cagis_addr)

print(d, n = 100)

# define match result category
d <- d |>
  mutate(
    addr_match_result =
      case_when(
        purrr::map_lgl(cagis_addr_matches, is.null) ~ NA,
        purrr::map_dbl(cagis_addr_matches, vctrs::vec_size) == 0 ~ "no_match",
        purrr::map_dbl(cagis_addr_matches, vctrs::vec_size) == 1 ~ "single_match",
        purrr::map_dbl(cagis_addr_matches, \(.) length(unique(.))) == 1 ~ "multi_match_identical",
        purrr::map_dbl(cagis_addr_matches, vctrs::vec_size) > 1 ~ "multi_match",
        .default = "foofy"
      ) |>
      factor(levels = c("no_match", "single_match", "multi_match_identical", "multi_match"))
  )

summary(d$addr_match_result)

d |>
  filter(addr_match_result == "multi_match") |>
  select(clean_address, cagis_addr_matches) |>
  tibble::deframe()

## street names shorter than 4 characters should not be considered a match unless oas distance == 0
## (e.g., "117 12th Street" and "117 13th Street")

## "Forestview Court" should not match with "Forestview Lane" in the same zipcode; use other dist instead of osa?

matched_addr_data <-
  d |>
  filter(addr_match_result %in% c("single_match")) |>
  tidyr::unnest(cols = c("cagis_addr_matches")) |>
  mutate(.tmp = vec_cast.addr.character(cagis_addr_matches)) |>
  left_join(mutate(cagis_addr, .tmp = vec_cast.addr.character(.data$cagis_addr)),
    by = ".tmp"
  ) |>
  select(-cagis_addr_matches, -.tmp)
