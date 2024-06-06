test_that("addr_standardize works", {
  addr_standardize(
    x = c(
      "3333 Burnet Avenue Apt 2 Cincinnati OH 45220",
      "3333 bUrNeT Avenue Cincinnati OH 45220",
      "3333 Burnet Avenue Apt #2 Cincinnati OH 45220",
      "3333 Burnet Ave Cincinnati OH 45220",
      "3333 Burnet Av. Cincinnati OH 45220",
      "3333 Burnet Avenue Cincinnati 45220"
    )
  ) |>
    expect_equal(c(rep("3333 burnet avenue cincinnati oh 45220", 5), "3333 burnet avenue cincinnati na 45220"))
})

test_that("addr_standardize works with tricky addresses", {
  c(
    "202 Riva Ridge Ct Cincinnati OH 45140", # more than one word in street name
    "3333 Burnet Ave San Francisco OH 45219"
  ) |>
    addr_standardize() |>
    expect_equal(c(
      "202 riva ridge court cincinnati oh 45140",
      "3333 burnet avenue san francisco oh 45219"
    ))
})

test_that("expand_post_type works", {
  expand_post_type(c("ave", "av", "avenue", "st", NA, "")) |>
    expect_equal(c("Avenue", "Avenue", "Avenue", "Street", NA, ""))
})
