test_that("addr() works", {
  addr(c("3333 Burnet Ave Cincinnati OH 45219", "202 Riva Ridge Ct Cincinnati OH 45140")) |>
    testthat::expect_no_error() |>
    expect_s3_class("addr") |>
    vctrs::vec_data() |>
    expect_equal(
      structure(list(street_number = c(3333L, 202L), street_name = c(
        "burnet",
        "riva ridge"
      ), street_type = c("avenue", "court"), city = c(
        "cincinnati",
        "cincinnati"
      ), state = c("oh", "oh"), zip_code = c("45219", "45140")), class = "data.frame", row.names = c(NA, -2L))
    )
})

# TODO test printing:
addr(c("3333 Burnet Ave Cincinnati OH 45219", "202 Riva Ridge Ct Cincinnati OH 45140"))
