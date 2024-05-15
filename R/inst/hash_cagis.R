devtools::load_all()
library(dplyr)

d <- 
  fr::read_fr_tdr(fs::path_package("parcel", "cagis_parcels")) |>
  tibble::as_tibble() |>
  select(parcel_id, address = parcel_address)

multi_extract <- function(x, name) {paste(x[names(x) == name], collapse = " ")}

d <- d |>
  mutate(addr_tags = addr_tag(address),
         addr_number = purrr::map_chr(addr_tags, multi_extract, "AddressNumber"),
         addr_street = purrr::map_chr(addr_tags, multi_extract, "StreetName"),
         addr_type = purrr::map_chr(addr_tags, multi_extract, "StreetNamePostType"),
         addr_zip = "00000")

d <-
  d |>
  rowwise() |>
  mutate(addr = paste(addr_number,
                      tolower(addr_street),
                      expand_post_type(tolower(addr_type)),
                      substr(addr_zip, 1, 5))) |>
  ungroup()


# add hashdress
d$hashdress <- vapply(d$addr, \(.) digest::digest(., algo = "md5", serialize = FALSE), character(1), USE.NAMES = FALSE)
# NA_character_ is still hashed; change these back to NA
d[d$hashdress == digest::digest(NA_character_, algo = "md5", serialize = FALSE), "hashdress"] <- NA_character_

d |> sample_n(10)
