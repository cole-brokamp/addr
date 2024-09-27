
<!-- README.md is generated from README.Rmd. Please edit that file -->

# addr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/hashdress)](https://CRAN.R-project.org/package=hashdress)
[![R-CMD-check](https://github.com/cole-brokamp/addr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cole-brokamp/addr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Addresses that were not validated at the time of collection are often
heterogenously formatted, making them difficult to directly compare. The
goal of addr is to clean, parse, standardize, and match messy,
real-world addresses in R to use for data linkages.

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

## Using

### addr vectors in R

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

or

``` r
addr(c("3333 Burnet Ave Cincinnati OH 45229", "202 Riva Ridge Ct Cincinnati OH 45140")) |>
  as.character()
#> [1] "3333 Burnet Avenue Cincinnati OH 45229"  
#> [2] "202 Riva Ridge Court Cincinnati OH 45140"
```

### addr matching

List all of the potentially matching `addr`s in a reference set of
`addr`s with `addr_match()`. The code below matches input addresses to
the reference set of all addresses in Hamilton County, OH included in
the package:

``` r
addr(c("3333 Burnet Ave Cincinnati OH 45229", 
    "5130 RAPID RUN RD CINCINNATI OHIO 45238",
    "5131 RAPID RUN RD CINCINNATI OHIO 45238"
)) |>
  addr_match(cagis_addr()$cagis_addr)
#> <addr[3]>
#> [1] 3333 Burnet Avenue Cincinnati OH 45229     
#> [2] 5130 Rapid Run Road Delhi Township OH 45238
#> [3] NA
```

Use the matched addr vector to merge in address-specific data in the
included `cagis_addr` object.

``` r
addr(c("3333 Burnet Ave Cincinnati OH 45229", "5130 RAPID RUN RD CINCINNATI OHIO 45238")) |>
  addr_match(cagis_addr()$cagis_addr) |>
  tibble::enframe(name = "input_addr", value = "ca") |>
  dplyr::left_join(cagis_addr(), by = c("ca" = "cagis_addr"))
#> # A tibble: 2 × 3
#>   input_addr                                          ca    cagis_addr_data
#>        <int>                                      <addr> <list<tibble[,6]>>
#> 1          1      3333 Burnet Avenue Cincinnati OH 45229            [1 × 6]
#> 2          2 5130 Rapid Run Road Delhi Township OH 45238            [1 × 6]
```

If exact matching fails, use matching to TIGER street range files from
the US census:

``` r
addr(c("3333 Burnet Ave Cincinnati OH 45229", "5130 RAPID RUN RD CINCINNATI OHIO 45238")) |>
  addr_match_tiger_street_ranges()
#> $`3333 Burnet Avenue Cincinnati OH 45229`
#> # A tibble: 2 × 4
#>   TLID      s2_geography                                            from    to
#>   <chr>     <s2_geography>                                         <dbl> <dbl>
#> 1 103925697 LINESTRING (-84.500403 39.14089, -84.500289 39.141892)  3301  3399
#> 2 103925699 LINESTRING (-84.500525 39.139737, -84.500403 39.14089)  3247  3398
#> 
#> $`5130 Rapid Run Road Cincinnati OHIO 45238`
#> # A tibble: 1 × 4
#>   TLID      s2_geography                                              from    to
#>   <chr>     <s2_geography>                                           <dbl> <dbl>
#> 1 650346231 LINESTRING (-84.608444 39.110496, -84.6087 39.110523, -…  5094  5199
```

Because the addresses are possibly located on more than one street range
geography, use the `summarize` argument to return the centroid of each
set of matched street ranges and then add TIGER/Line census block group
identifers via geospatial intersection:

``` r
addr(c("3333 Burnet Ave Cincinnati OH 45229", "5130 RAPID RUN RD CINCINNATI OHIO 45238")) |>
  addr_match_tiger_street_ranges(county = "39061", summarize = "centroid") |>
  dplyr::bind_rows() |>
  dplyr::mutate(census_bg_id = tiger_block_groups(s2::as_s2_cell(s2_geography)))
#> # A tibble: 2 × 5
#>   TLID                s2_geography                    from    to census_bg_id
#>   <chr>               <s2_geography>                 <dbl> <dbl> <chr>       
#> 1 103925697-103925699 POINT (-84.5004091 39.1408146)  3247  3399 390610270002
#> 2 650346231           POINT (-84.6103702 39.1110311)  5094  5199 390610214011
```

The above process is conducted, with default matching arguments, in the
function `addr_match_geocode`, which requires a vector of reference s2
cell locations: **As of now, this process only works with the `cagis_s2`
available as below and for matching within Hamilton County, OH (36061)
using 2022 TIGER street range files**

``` r

# select one s2 cell at random from addresses with more than one parcel identifier and coordinates
cagis_s2 <-
   cagis_addr()$cagis_addr_data |>
   purrr::modify_if(\(.) length(.) > 0 && nrow(.) > 1, dplyr::slice_sample, n = 1) |>
   purrr::map_vec(purrr::pluck, "cagis_s2", .default = NA, .ptype = s2::s2_cell())
   
addr_match_geocode(x = sample(voter_addresses(), 100), 
                   ref_addr = cagis_addr()$cagis_addr,
                   ref_s2 = cagis_s2,
                   county = "39061",
                   year = "2022")
#> # A tibble: 100 × 3
#>                                           addr s2               match_method
#>                                         <addr> <s2cell>         <fct>       
#>  1     411 Pedretti Avenue Cincinnati OH 45238 8841b6192539a2e5 ref_addr    
#>  2    7432 Bayswater Drive Cincinnati OH 45255 8841af3264f87405 tiger_range 
#>  3      5366 Dickens Drive Cincinnati OH 45241 884051140ea170a7 ref_addr    
#>  4        245 Mcguire Lane Cincinnati OH 45215 88405286586bc095 ref_addr    
#>  5     3909 Dickson Avenue Cincinnati OH 45229 8841b30e55fab54d ref_addr    
#>  6      4025 Egbert Avenue Cincinnati OH 45220 8841b37c962f5a3b ref_addr    
#>  7   11451 Folkstone Drive Cincinnati OH 45240 88404ebd7161497d ref_addr    
#>  8  1860 Queen City Avenue Cincinnati OH 45214 8841b43472fe3997 ref_addr    
#>  9 9797 Cooper Woods Court Cincinnati OH 45241 88405251fbc59f91 ref_addr    
#> 10       3815 Lincoln Road Cincinnati OH 45247 88403554febe6d93 ref_addr    
#> # ℹ 90 more rows
```
