devtools::load_all()
library(dplyr, warn.conflicts = FALSE)

d <-
  tibble::tibble(address = voter_addresses()) |>
  mutate(addr = as_addr(address))

cagis_s2 <-
  cagis_addr()$cagis_addr_data |>
  purrr::modify_if(\(.) length(.) > 0 && nrow(.) > 1, dplyr::slice_sample, n = 1) |>
  purrr::map_vec(purrr::pluck, "cagis_s2", .default = NA, .ptype = s2::s2_cell())

d_amg <-
  addr_match_geocode(
    d$addr,
    ref_addr = cagis_addr()$cagis_addr, ref_s2 = cagis_s2,
    county = "39061", year = "2022"
  )

saveRDS(d, "inst/voter_geocode_addr.rds")
