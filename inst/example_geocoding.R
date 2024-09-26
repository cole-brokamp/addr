devtools::load_all()
library(dplyr, warn.conflicts = FALSE)

length(voter_addresses())
# 242,133

ia <-
  voter_addresses() |>
  as_addr()

d <- tibble::tibble(addr = ia)

d$matched_cagis_addr <-
  addr_match(ia,
    cagis_addr()$cagis_addr,
    stringdist_match = "osa_lt_1",
    match_street_type = TRUE,
    simplify = TRUE
  )

# randomly select one s2 cell if more than one lat/lon was present for the same address (but different parcel_id)
set.seed(0)

cagis_matches <- match(as.character(d$matched_cagis_addr), as.character(cagis_addr()$cagis_addr))

d$s2 <-
  cagis_addr()$cagis_addr_data[cagis_matches] |>
  purrr::modify_if(\(.) length(.) > 0 && nrow(.) > 1, dplyr::slice_sample, n = 1) |>
  purrr::map_vec(purrr::pluck, "cagis_s2", .default = NA, .ptype = s2::s2_cell())

# try to street range matched un matched addresses
cagis_no_match <- is.na(d$s2)

message(
  cli::symbol$arrow_right, " ",
  scales::percent(sum(!cagis_no_match) / length(cagis_no_match)),
  " (n = ", sum(!cagis_no_match), ") matched to exact parcel address"
)

t_matches <-
  addr_match_tiger_street_ranges(d[cagis_no_match, "addr", drop = TRUE], "39061", summarize = "centroid", street_only_match = TRUE) |>
  purrr::discard(\(.) length(.) < 1) |> # removes NULL
  purrr::discard(\(.) nrow(.) < 1) # removes empty data.frame

t_match_addr <- as_addr(names(t_matches))
t_match_s2 <- purrr::map_vec(t_matches, \(.) s2::as_s2_cell(.$s2_geography), .ptype = s2::s2_cell())

message(
  cli::symbol$arrow_right, " ",
  scales::percent(length(t_matches) / sum(cagis_no_match)),
  " (n = ", length(t_matches), ") of unmatched addresses (n = ", sum(cagis_no_match), ") matched with tiger street range centroids"
)

d[d$addr %in% t_match_addr, "s2"] <- t_match_s2

message(
  cli::symbol$arrow_right, " ",
  scales::percent(sum(is.na(d$s2)) / length(cagis_no_match)),
  " (n = ", sum(is.na(d$s2)), ") of all addresses could not be matched"
)

saveRDS(d, "inst/voter_geocode_addr.rds")
