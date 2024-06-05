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
    expect_equal(c(rep("3333 burnet avenue cincinnati oh 45220", times = 5), NA))
})

test_that("addr_standardize works with tricky addresses", {

  x <- c(
    "202 Riva Ridge Ct Cincinnati OH 45140", # more than one word in street name
    "3333 Burnet Ave San Francisco OH 45219" 
    )
    

})

test_that("expand_post_type works", {
  expect_equal(expand_post_type("ave"), "avenue")
  expect_equal(expand_post_type("av"), "avenue")
  expect_equal(expand_post_type("av"), "avenue")
  expect_equal(expand_post_type("avenue"), "avenue")
  expect_equal(expand_post_type("st"), "street")
  expect_equal(expand_post_type(NULL), NULL)
})

