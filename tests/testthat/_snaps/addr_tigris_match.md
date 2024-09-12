# addr_match_tigris_street_ranges() works

    Code
      addr_match_tigris_street_ranges(as_addr(c("224 Woolper Ave", "3333 Burnet Ave",
        "33333 Burnet Ave", "609 Walnut St")))
    Output
      $`224 Woolper Avenue`
      # A tibble: 1 x 4
        TLID      s2_geography                                              from    to
        <chr>     <s2_geography>                                           <dbl> <dbl>
      1 103924294 LINESTRING (-84.511197 39.149684, -84.511752 39.149788,~   100   299
      
      $`3333 Burnet Avenue`
      # A tibble: 2 x 4
        TLID      s2_geography                                            from    to
        <chr>     <s2_geography>                                         <dbl> <dbl>
      1 103925697 LINESTRING (-84.500403 39.14089, -84.500289 39.141892)  3301  3399
      2 103925699 LINESTRING (-84.500525 39.139737, -84.500403 39.14089)  3247  3398
      
      $`33333 Burnet Avenue`
      # A tibble: 0 x 4
      # i 4 variables: TLID <chr>, s2_geography <s2_geography>, from <dbl>, to <dbl>
      
      $`609 Walnut Street`
      NULL
      

