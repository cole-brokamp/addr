test_that("get_tigris_street_ranges() works", {
  d <- get_tigris_street_ranges(county = "39061", year = "2022")
  expect_identical(nrow(d), 9267L)
  expect_identical(names(d), c("FULLNAME", "sf_tbl"))

  expect_identical(names(d$sf_tbl[[3]]), c("TLID", "geometry", "from", "to"))

  d$sf_tbl[[3]] |>
    expect_s3_class(c("sf")) |>
    nrow() |>
    expect_identical(4L)

})

test_that("addr_match_tigris_street_ranges() works", {

  addr_match_tigris_street_ranges(as_addr(c("224 Woolper Ave", "3333 Burnet Ave", "33333 Burnet Ave", "609 Walnut St"))) |>
    expect_snapshot()

})
