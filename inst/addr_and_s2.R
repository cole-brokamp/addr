devtools::load_all()
library(s2)

cagis_s2 <-
  cagis_addr()$cagis_addr_data |>
  purrr::modify_if(\(.) length(.) > 0 && nrow(.) > 1, dplyr::slice_sample, n = 1) |>
  purrr::map_vec(purrr::pluck, "cagis_s2", .default = NA, .ptype = s2::s2_cell())

set.seed(1)
d <- addr_match_geocode(
  x = sample(voter_addresses(), 200),
  ref_addr = cagis_addr()$cagis_addr,
  ref_s2 = cagis_s2,
  county = "39061",
  year = "2022"
)

table(d$match_method)

d <-
  dplyr::filter(d, match_method %in% c("ref_addr", "tiger_range"))

d$coarse_s2 <- s2_cell_parent(d$s2, 15)
d$coarse_s2_char <- as.character(d$coarse_s2)

# median area in sq km
median(s2_cell_area_approx(d$coarse_s2)) / 1000000

d$geometry <- sf::st_as_sfc(s2_cell_center(d$s2))

bg <-
  get_tiger_block_groups("39", "2022") |>
  dplyr::filter(substr(GEOID, 1, 5) == "39061") |>
  dplyr::mutate(geometry = sf::st_as_sfc(s2_geography))

s2_plot(bg$s2_geography, border = codec::codec_colors("grey blue"), col = codec::codec_colors("white"))
s2::s2_plot(s2_cell_to_lnglat(d$s2), add = TRUE, col = codec::codec_colors("darkish blue"), pch = 19)

library(rdeck)

rdeck(map_style = sprintf("mapbox://styles/mapbox/light-v%d", 10), initial_bounds = sf::st_bbox(bg$geometry)) |>
  add_polygon_layer(
    data = bg,
    name = "block groups",
    get_polygon = geometry,
    opacity = 0.2,
    filled = TRUE,
    stroked = TRUE,
    get_fill_color = codec::codec_colors("white"),
    get_line_color = codec::codec_colors("grey blue"),
    get_line_width = 50,
    visible = TRUE,
    visibility_toggle = FALSE,
    pickable = FALSE
  ) |>
  ## add_polygon_layer(
  ##   data = d,
  ##   name = "s2_cell_outlines",
  ##   get_polygon = coarse_geo
  ## ) |>
  add_s2_layer(
    data = d,
    name = "s2_cell_layer",
    get_s2_token = coarse_s2_char,
    opacity = 0.5,
    get_fill_color = codec::codec_colors("red")
  ) |>
  add_scatterplot_layer(
    name = "s2_cell_centers",
    data = d,
    opacity = 0.5,
    get_radius = 50,
    get_position = geometry,
    get_fill_color = codec::codec_colors("orange"),
  )
