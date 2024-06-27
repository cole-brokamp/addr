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
  filter(!clean_address == "")

# randomly select one address from 10% of all patients
d <- d |>
  group_by(MRN) |>
  slice_sample(n = 1) |>
  ungroup() |>
  slice_sample(prop = 0.1)

# convert address character strings into an addr vector
d$addr <- as_addr(d$clean_address)

# match with addr::cagis_addr reference addresses included in the package
d$cagis_addr_matches <- addr_match(d$addr, cagis_addr$cagis_addr)

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

# inspect multi_matches manually
d |>
  filter(addr_match_result == "multi_match") |>
  select(clean_address, cagis_addr_matches) |>
  tibble::deframe()

# retain only single matches and use a 
# hacky workaround to join with cagis_addr on addr by casting to character
matched_addr_data <-
  d |>
  filter(addr_match_result %in% c("single_match")) |>
  tidyr::unnest(cols = c("cagis_addr_matches")) |>
  mutate(.tmp = vec_cast.addr.character(cagis_addr_matches)) |>
  left_join(mutate(cagis_addr, .tmp = vec_cast.addr.character(.data$cagis_addr)),
    by = ".tmp"
  ) |>
  select(-cagis_addr_matches, -.tmp)
