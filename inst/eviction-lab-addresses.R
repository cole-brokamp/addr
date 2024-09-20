devtools::load_all()
library(dplyr, warn.conflicts = FALSE)

d <-
  readr::read_csv("https://evictionlab.org/uploads/cincinnati_hotspots_media_report.csv", show_col_types = FALSE) |>
  mutate(
    addr = as_addr(xstreet_clean),
    cagis_addr = addr_match_street_name_and_number(addr, cagis_addr()$cagis_addr, simplify = TRUE)
  )

# or could read in cagis addrs and create versions without city, state, and zip to match with addr_match()


print(d, n = 100)
