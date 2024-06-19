devtools::load_all()
library(dplyr, warn.conflicts = FALSE)

d <-
  fr::read_fr_tdr(fs::path_package("parcel", "cagis_parcels")) |>
  tibble::as_tibble() |>
  select(parcel_id, address = parcel_address)

d$addr <- addr_standardize(d$address, tags = c("AddressNumber", "StreetName", "StreetNamePostType"))

# add in city, state, and pseudo zipcode for matching

# add hashdress
d$hashdress <- vapply(d$addr, \(.) digest::digest(., algo = "md5", serialize = FALSE), character(1), USE.NAMES = FALSE)
# NA_character_ is still hashed; change these back to NA
d[d$hashdress == digest::digest(NA_character_, algo = "md5", serialize = FALSE), "hashdress"] <- NA_character_
