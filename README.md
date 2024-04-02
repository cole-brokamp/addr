
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

The goal of addr is to clean, parse, harmonize, and hash messy
real-world addresses in R.

Addresses that were not validated at the time of collection are often
heterogenously formatted, making them difficult to compare or link to
other sets of addresses. The addr package is designed to (1) clean
character strings of addresses, (2) use the `usaddress` library to tag
address components, (3) expand common street type abbreviations, and (4)
paste together select components to create a standardized address.
Standardized addresses can be hashed to create hashdresses that can be
used to merge with other sets of addresses.

## Installation

You can install the development version of addr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("cole-brokamp/addr")
```

## Example

``` r
library(addr)
```

Transform messy, real-world addresses into a character vector of
standardized address:

``` r
addr_standardize(
  x = c(
    "3333 Burnet Avenue Apt 2 Cincinnati OH 45220",
    "3333 bUrNeT Avenue Cincinnati OH 45220",
    "3333 Burnet Avenue Apt #2 Cincinnati OH 45220",
    "3333 Burnet Ave Cincinnati OH 45220",
    "3333 Burnet Av. Cincinnati OH 45220"
  )
)
#> [1] "3333 burnet avenue cincinnati oh 45220"
#> [2] "3333 burnet avenue cincinnati oh 45220"
#> [3] "3333 burnet avenue cincinnati oh 45220"
#> [4] "3333 burnet avenue cincinnati oh 45220"
#> [5] "3333 burnet avenue cincinnati oh 45220"
```

Use a hash representing the standardized address instead:

``` r
addr_hash(
  x = c(
    "3333 Burnet Avenue Apt 2 Cincinnati OH 45220",
    "3333 bUrNeT Avenue Cincinnati OH 45220",
    "3333 Burnet Avenue Apt #2 Cincinnati OH 45220",
    "3333 Burnet Ave Cincinnati OH 45220",
    "3333 Burnet Av. Cincinnati OH 45220"
  )
)
```

Use `addr_tag()` to generate tagged address components:

``` r
addr_tag(c("290 Ludlow Avenue Apt #2 Cincinnati OH 45220", "3333 Burnet Ave Cincinnati OH 45219"))
#> [[1]]
#>       AddressNumber          StreetName  StreetNamePostType       OccupancyType 
#>               "290"            "Ludlow"            "Avenue"               "Apt" 
#> OccupancyIdentifier           PlaceName           StateName             ZipCode 
#>                 "2"        "Cincinnati"                "OH"             "45220" 
#> 
#> [[2]]
#>      AddressNumber         StreetName StreetNamePostType          PlaceName 
#>             "3333"           "Burnet"              "Ave"       "Cincinnati" 
#>          StateName            ZipCode 
#>               "OH"            "45219"
```
