test_that("addr_standardize works", {
  addr_standardize(c("3333 Burnet Ave Cincinnati OH 45219", "202 Riva Ridge Ct Cincinnati OH 45140")) |>
    expect_equal(
      list(
        list(
          AddressNumber = "3333", StreetName = "Burnet", StreetNamePostType = "Avenue",
          PlaceName = "Cincinnati", StateName = "OH", ZipCode = "45219"
        ),
        list(
          AddressNumber = "202", StreetName = "Riva Ridge", StreetNamePostType = "Court",
          PlaceName = "Cincinnati", StateName = "OH", ZipCode = "45140"
        )
      )
    )

  addr_standardize(c("3333 Burnet Ave Cincinnati OH 45219", "202 Riva Ridge Ct Cincinnati OH 45140"),
    tags = c("AddressNumber", "StreetName", "StreetNamePostType")
  ) |>
    expect_equal(
      list(
        list(AddressNumber = "3333", StreetName = "Burnet", StreetNamePostType = "Avenue"),
        list(AddressNumber = "202", StreetName = "Riva Ridge", StreetNamePostType = "Court")
      )
    )

  test_that("expand_post_type works", {
    expand_post_type(c("ave", "av", "avenue", "st", NA, "")) |>
      expect_equal(c("Avenue", "Avenue", "Avenue", "Street", NA, ""))
  })
})
