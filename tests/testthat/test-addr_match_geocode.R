test_that("addr_match_geocode() works", {
  skip_if(testthat:::on_ci() && Sys.info()[["sysname"]] == "Linux", "Skipping test on CI on Linux")
  set.seed(0)
  cagis_s2 <-
    cagis_addr()$cagis_addr_data |>
    purrr::modify_if(\(.) length(.) > 0 && nrow(.) > 1, dplyr::slice_sample, n = 1) |>
    purrr::map_vec(purrr::pluck, "cagis_s2", .default = NA, .ptype = s2::s2_cell())
  my_geocodes <- addr_match_geocode(x = sample(voter_addresses(), 250), ref_s2 = cagis_s2)

  table(my_geocodes$match_method) |>
    expect_identical(structure(c(ref_addr = 225L, tiger_range = 9L, none = 16L), dim = 3L, dimnames = structure(list(
      c("ref_addr", "tiger_range", "none")
    ), names = ""), class = "table"))
})
