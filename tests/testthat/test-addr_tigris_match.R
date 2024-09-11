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
