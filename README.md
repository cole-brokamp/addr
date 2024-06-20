
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/hashdress)](https://CRAN.R-project.org/package=hashdress)
[![R-CMD-check](https://github.com/cole-brokamp/addr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cole-brokamp/addr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# addr

<!-- badges: start -->
<!-- badges: end -->

Addresses that were not validated at the time of collection are often
heterogenously formatted, making them difficult to compare or link to
other sets of addresses. The goal of addr is to clean, parse, and
standardize messy, real-world addresses in R to use for further linkage
and lookup.

## Installation

You can install the development version of addr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("cole-brokamp/addr")
```

addr requires a working
[Rust](https://www.rust-lang.org/learn/get-started) toolchain; install
one using [rustup](https://www.rust-lang.org/tools/install).

## Example

``` r
library(addr)
```

``` r
addr_standardize(c("3333 Burnet Ave Cincinnati OH 45219", "202 Riva Ridge Ct Cincinnati OH 45140"))
#> [[1]]
#> [[1]]$AddressNumber
#> [1] "3333"
#> 
#> [[1]]$StreetName
#> [1] "Burnet"
#> 
#> [[1]]$StreetNamePostType
#> [1] "Avenue"
#> 
#> [[1]]$PlaceName
#> [1] "Cincinnati"
#> 
#> [[1]]$StateName
#> [1] "OH"
#> 
#> [[1]]$ZipCode
#> [1] "45219"
#> 
#> 
#> [[2]]
#> [[2]]$AddressNumber
#> [1] "202"
#> 
#> [[2]]$StreetName
#> [1] "Riva Ridge"
#> 
#> [[2]]$StreetNamePostType
#> [1] "Court"
#> 
#> [[2]]$PlaceName
#> [1] "Cincinnati"
#> 
#> [[2]]$StateName
#> [1] "OH"
#> 
#> [[2]]$ZipCode
#> [1] "45140"
```
