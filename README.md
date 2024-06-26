
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

The `addr` package provides the `addr` R object, which stores
standardized address tags, but acts like a usual vector in R:

``` r
library(addr)
```

``` r
addr(c("3333 Burnet Ave Cincinnati OH 45229", "202 Riva Ridge Ct Cincinnati OH 45140"))
#> <addr[2]>
#> [1] 3333 Burnet Avenue Cincinnati OH 45229  
#> [2] 202 Riva Ridge Court Cincinnati OH 45140
```

Under the hood, an `addr` vector keeps a record of the tagged and
standardized address components so that they can be used with other
functions. To inspect or use them directly:

``` r
addr(c("3333 Burnet Ave Cincinnati OH 45229", "202 Riva Ridge Ct Cincinnati OH 45140")) |>
  as.data.frame()
#>   street_number street_name street_type       city state zip_code
#> 1          3333      burnet      avenue cincinnati    oh    45229
#> 2           202  riva ridge       court cincinnati    oh    45140
```

List all of the potentially matching `addr`s in a reference set of
`addr`s with `addr_match()`. The code below matches two addresses to the
reference set of all addresses in Hamilton County, OH included in the
package:

``` r
addr(c("3333 Burnet Ave Cincinnati OH 45229", 
    "5130 RAPID RUN RD CINCINNATI OHIO 45238",
    "5131 RAPID RUN RD CINCINNATI OHIO 45238"
)) |>
  addr_match(cagis_addr$cagis_addr)
#> $`3333 Burnet Avenue Cincinnati OH 45229`
#> <addr[1]>
#> [1] 3333 Burnet Avenue Cincinnati OH 45229
#> 
#> $`5130 Rapid Run Road Cincinnati OHIO 45238`
#> <addr[1]>
#> [1] 5130 Rapid Run Road Delhi Township OH 45238
#> 
#> $`5131 Rapid Run Road Cincinnati OHIO 45238`
#> <addr[0]>
```
