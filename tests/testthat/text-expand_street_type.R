test_that("expand_post_type works", {
  expand_post_type(c("ave", "av", "avenue", "Avenue", "st", NA, "")) |>
    expect_equal(c("avenue", "avenue", "avenue", "avenue", "street", NA, ""))
})
