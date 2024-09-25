test_that("tiger_block_groups works", {
  tiger_block_groups(x = s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")), year = "2023") |>
    expect_identical(c("390610032001", "210370519034"))
})

test_that("tiger_block_groups works with NAs", {
  tiger_block_groups(x = s2::as_s2_cell(c("8841b39a7c46e25f", NA, "8841a45555555555")), year = "2023") |>
    expect_identical(c("390610032001", NA, "210370519034"))
})
