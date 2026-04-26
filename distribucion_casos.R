library(mapgl)
library(fishualize)
library(sf)

load("~/Library/CloudStorage/Dropbox/hotspots_2025/8.RData/denmex_chains.RData")
load("/Users/fdzul/Documents/geocoding_mx_2026_dengue/4.RData/dengue_mx_2026.RData")
source("~/Library/CloudStorage/Dropbox/r_developments/r_dashboards/netlify/1.2.persisten_dengue_transmission_2026/persistent_dengue_transmission_10_durango/mp_transmission_chains.R")
source("~/Library/CloudStorage/Dropbox/r_developments/r_dashboards/netlify/1.2.persisten_dengue_transmission_2026/persistent_dengue_transmission_10_durango/mp_dengue_cases.R")
names(xy)
names(z)
z |>
    dplyr::filter(IDE_EDA_ANO <= 12 | IDE_EDA_ANO >= 65) |>
    dplyr::mutate(onset = FEC_INI_SIGNOS_SINT) |>
    dplyr::select(ANO, long, lat, onset) |>
    dplyr::mutate(x = long,
                  y = lat) |>
    sf::st_as_sf(coords = c("x", "y"),
                 crs = 4326) |>
    mp_dengue_cases(yrs = 2026,
                    locality = "Hermosillo",
                    cve_edo = "26")
source("~/Library/CloudStorage/Dropbox/r_developments/r_dashboards/netlify/1.2.persisten_dengue_transmission_2026/persistent_dengue_transmission_10_durango/mp_dengue_cases.R")
library(mapgl)
library(fishualize)
library(sf)
mp_dengue_cases(data = xy,
                yrs = c(2016:2022),
                locality = "Victoria de Durango",
                cve_edo = "10")