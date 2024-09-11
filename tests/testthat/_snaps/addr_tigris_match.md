# addr_match_tigris_street_ranges() works

    Code
      addr_match_tigris_street_ranges(as_addr(c("224 Woolper Ave", "3333 Burnet Ave",
        "33333 Burnet Ave", "609 Walnut St")))
    Output
      $`224 Woolper Avenue`
      Simple feature collection with 1 feature and 3 fields
      Geometry type: LINESTRING
      Dimension:     XY
      Bounding box:  xmin: -84.51845 ymin: 39.14968 xmax: -84.5112 ymax: 39.15023
      Geodetic CRS:  NAD83
      # A tibble: 1 x 4
        TLID                                                      geometry  from    to
      * <chr>                                             <LINESTRING [°]> <dbl> <dbl>
      1 103924294 (-84.5112 39.14968, -84.51175 39.14979, -84.51201 39.14~   100   299
      
      $`3333 Burnet Avenue`
      Simple feature collection with 2 features and 3 fields
      Geometry type: LINESTRING
      Dimension:     XY
      Bounding box:  xmin: -84.50052 ymin: 39.13974 xmax: -84.50029 ymax: 39.14189
      Geodetic CRS:  NAD83
      # A tibble: 2 x 4
        TLID                                     geometry  from    to
      * <chr>                            <LINESTRING [°]> <dbl> <dbl>
      1 103925697 (-84.5004 39.14089, -84.50029 39.14189)  3301  3399
      2 103925699 (-84.50052 39.13974, -84.5004 39.14089)  3247  3398
      
      $`33333 Burnet Avenue`
      Simple feature collection with 0 features and 3 fields
      Bounding box:  xmin: NA ymin: NA xmax: NA ymax: NA
      Geodetic CRS:  NAD83
      # A tibble: 0 x 4
      # i 4 variables: TLID <chr>, geometry <GEOMETRY [°]>, from <dbl>, to <dbl>
      
      $`609 Walnut Street`
      NULL
      

