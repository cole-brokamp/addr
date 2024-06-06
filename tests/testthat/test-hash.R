test_that("addr_hash works", {
  addr_hash(
    x = c(
      "3333 Burnet Avenue Apt 2 Cincinnati OH 45220",
      "3333 bUrNeT Avenue Cincinnati OH 45220",
      "3333 Burnet Avenue Apt #2 Cincinnati OH 45220",
      "3333 Burnet Ave Cincinnati OH 45220",
      "3333 Burnet Av. Cincinnati OH 45220",
      "3333 Burnet Avenue Cincinnati 45220"
    )
  ) |>
    expect_equal(c(rep("da219816d9cb3e1bb53291312cfa1dfd", times = 5), "0b96d7ca8835e109a5acc9d0497456a7"))
})
