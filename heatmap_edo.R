cve_edo = "26"
library(sf)
library(mapgl)
yrs = 2026
load("/Users/fdzul/Documents/geocoding_mx_2026_dengue/4.RData/dengue_mx_2026.RData")
source("~/Library/CloudStorage/Dropbox/r_developments/r_dashboards/netlify/2.active_transmission/2026/active_dengue_transmission_2026_26_sonora/mp_heatmap.R")
z |>
    dplyr::filter(IDE_EDA_ANO <= 12 | IDE_EDA_ANO >= 65) |>
    dplyr::mutate(onset = FEC_INI_SIGNOS_SINT) |>
    dplyr::select(ANO, long, lat, onset) |>
    dplyr::mutate(x = long,
                  y = lat) |>
    sf::st_as_sf(coords = c("x", "y"),
                 crs = 4326) |>
    dplyr::mutate(month = lubridate::month(onset)) |>
    dplyr::filter(month %in% c(1:3)) |>
    mp_heatmap(yrs = 2026,
           cve_edo = "26",
           locality = "Hermosillo",
           state = TRUE)

# Step 1. transform dataset #####
z <- z |>
    dplyr::filter(IDE_EDA_ANO <= 12 | IDE_EDA_ANO >= 65) |>
    dplyr::mutate(onset = FEC_INI_SIGNOS_SINT) |>
    dplyr::select(ANO, long, lat, onset) |>
    dplyr::mutate(x = long,
                  y = lat) |>
    sf::st_as_sf(coords = c("x", "y"),
                 crs = 4326) 

# Step 2. extract the locality ####
loc <- rgeomex::AGEE_inegi19_mx|>
    dplyr::select(-NOMGEO) |>
    dplyr::filter(CVE_ENT %in% c(cve_edo))

# Step 3. extract the geocoded cases of merida ####

z <- z[loc, ]  |>
    dplyr::mutate(x = long,
                  y = lat) |>
    #sf::st_drop_geometry() |>
    dplyr::filter(ANO %in% c(yrs))

#########

mapgl::maplibre_view(loc,
                     color = NA,
                     #style = carto_style(style_name = "voyager")
                     style = mapgl::carto_style(style_name = "positron")) |>
    mapgl::add_source("area",  data = loc) |>
    mapgl::add_source("casos", data = z) |>
    # Límites de la ciudad
    add_fill_layer(id           = "ciudad-fill",
                   source       = "area",
                   fill_color   = "transparent",
                   fill_opacity = 0) |>
    mapgl::add_line_layer(id             = "ciudad-borde",
                          source         = "area",
                          line_color     = "#444444",
                          #line_dasharray = c(1, 1)
                          line_width     = 1) |>
    mapgl::add_heatmap_layer(id = "dengue_cases",
                             source = "casos",
                             heatmap_weight = mapgl::interpolate(column = "mag",
                                                                 values = c(0, 6),
                                                                 stops = c(0, 1)),
                             heatmap_intensity = mapgl::interpolate(property = "zoom",
                                                                    values = c(0, 9),
                                                                    stops = c(1, 3)),
                             heatmap_color = mapgl::interpolate(property = "heatmap-density",
                                                                values = seq(from = 0, to = 1, by = 0.2),
                                                                stops = c('rgba(33,102,172,0)', 
                                                                          'rgb(103,169,207)',
                                                                          'rgb(209,229,240)', 
                                                                          'rgb(253,219,199)',
                                                                          'rgb(239,138,98)', 
                                                                          'rgb(178,24,43)')),
                             heatmap_opacity = 0.7) |>
    mapgl::add_fullscreen_control(position = "top-left") |> 
    mapgl::add_navigation_control() |>
    mapgl::add_globe_control() |>
    mapgl::add_scale_control()
