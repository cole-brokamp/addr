test_that("get_tigris_street_ranges() works", {
  d <- get_tigris_street_ranges(county = "39061", year = "2022")
  expect_identical(length(d), 9267L)

  expect_identical(names(d[[1]]), c("TLID", "s2_geography", "from", "to"))

  d[[3]] |>
    expect_s3_class(c("tbl_df")) |>
    nrow() |>
    expect_identical(4L)

})

test_that("addr_match_tigris_street_ranges() works", {

  addr_match_tigris_street_ranges(as_addr(c("224 Woolper Ave", "3333 Burnet Ave", "33333 Burnet Ave", "609 Walnut St"))) |>
    expect_snapshot()

})
