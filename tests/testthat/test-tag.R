test_that("addr_tag works", {
  addr_tag(c("224 Woolper Avenue Apt #2 Cincinnati OH 45220", "3333 Burnet Ave Cincinnati OH 45219")) |>
    expect_equal(
      list(c(AddressNumber = "224", StreetName = "Woolper", StreetNamePostType = "Avenue", 
             OccupancyType = "Apt", OccupancyIdentifier = "2", PlaceName = "Cincinnati", 
             StateName = "OH", ZipCode = "45220"), c(AddressNumber = "3333", 
                                                     StreetName = "Burnet", StreetNamePostType = "Ave", PlaceName = "Cincinnati", 
                                                     StateName = "OH", ZipCode = "45219"))
    )
})
