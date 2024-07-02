test_that("addr_match works", {

  addr_match(
    addr(c("222 E Central Parkway Cincinnati OH 45000",
           "222 East Central Parkway Cincinnati OH 45000",
           "222 East Central Cincinnati OH 45000")),
    addr("222 E CENTRAL PKWY CINCINNATI OH 45000")
  ) |>
    purrr::list_c(ptype = addr()) |>
    unique() |>
    expect_equal(addr("222 E CENTRAL PKWY CINCINNATI OH 45000"))

  addr_match(
    addr(c(
      "14 E 14th street cincinnati oh 45000", "14 east 14th street cincinnati oh 45000", "14 W 14th street cincinnati oh 45000",
      "3333 Burnet Ave cincinnati oh 45000"
    )),
    addr(c("14 E 14TH STREET CINCINNATI OH 45000", "14 W 14TH STREET CINCINNATI OH 45000"))
  ) |>
    expect_equal(
      list(`14 E 14th Street Cincinnati OH 45000` = structure(list(
        street_number = 14, street_name = "e 14th", street_type = "street",
        city = "cincinnati", state = "oh", zip_code = "45000"
      ), class = c(
        "addr",
        "vctrs_rcrd", "vctrs_vctr"
      )), `14 E 14th Street Cincinnati OH 45000` = structure(list(
        street_number = 14, street_name = "e 14th", street_type = "street",
        city = "cincinnati", state = "oh", zip_code = "45000"
      ), class = c(
        "addr",
        "vctrs_rcrd", "vctrs_vctr"
      )), `14 W 14th Street Cincinnati OH 45000` = structure(list(
        street_number = 14, street_name = "w 14th", street_type = "street",
        city = "cincinnati", state = "oh", zip_code = "45000"
      ), class = c(
        "addr",
        "vctrs_rcrd", "vctrs_vctr"
      )), `3333 Burnet Avenue Cincinnati OH 45000` = structure(list(
        street_number = numeric(0), street_name = character(0), street_type = character(0),
        city = character(0), state = character(0), zip_code = character(0)
      ), class = c(
        "addr",
        "vctrs_rcrd", "vctrs_vctr"
      )))
    )
})


test_that("addr_match with cagis works", {

  my_addresses <- c(
    "781 GREENWOOD AVE APT 1 CINCINNATI OHIO 45229",
    "515 FOREST AVE CINCINNATI OHIO 45229",
    "1540 DUDLEY WALK APT F CINCINNATI OHIO 45214",
    "3333 BURNET AVE CINCINNATI OH 45219",
    "3333 BURNET AVE CINCINNATI OH 45229",
    "806 BLAIR AVE APT 12 CINCINNATI OHIO 45229",
    "300 OAK CREEK CT 13 FAIRFIELD OHIO 45014",
    "5130 RAPID RUN RD CINCINNATI OHIO 45238",
    "5131 RAPID RUN RD CINCINNATI OHIO 45238",
    "2583 RIVERSIDE DR CINCINNATI OHIO 45202",
    "7839 DAWN RD APT 6 CINCINNATI OHIO 45237",
    "222 EAST CENTRAL PKWY CINCINNATI OH 45202", # JFS
    "222 E CENTRAL PKWY CINCINNATI OHIO 45202", # JFS
    "222 CENTRAL PKWY CINCINNATI OHIO 45202", # JFS
    "4571 TIMBERLAKE DR BATAVIA OHIO 45103", # outside reference addr zip codes
    "31 HIGHRIDGE DR LOVELAND OHIO 45140",
    "117 E 12TH ST CINCINNATI, OH 45202" # Greater Cinti Coalition for The Homeless
  )

  cagis_matches <- addr_match(addr(my_addresses), cagis_addr$cagis_addr)

  expect_type(cagis_matches, "list")
  expect_snapshot(cagis_matches)
})
