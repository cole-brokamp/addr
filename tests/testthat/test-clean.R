test_that("addr_clean works", {
  clean_address_text(c(
    "3333 Burnet Ave Cincinnati OH 45219",
    "33_33 Burnet Ave. Cincinnati OH 45219",
    "33\\33 B\"urnet Ave; Ci!ncinn&*ati OH 45219",
    "3333 Burnet Ave Cincinnati OH 45219",
    "33_33 Burnet Ave. Cincinnati OH 45219"
  )) |>
    expect_equal(rep("3333 Burnet Ave Cincinnati OH 45219", times = 5))
})
