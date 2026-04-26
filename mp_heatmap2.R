mp_heatmap2 <- function(geocoded_dataset,
                       yrs,
                       cve_edo,
                       locality,
                       unique_id = NULL) {  # Añadir parámetro para ID único
    
    # Step 1. transform dataset #####
    z <- geocoded_dataset 
    
    # Step 2. extract the locality ####
    loc <- rgeomex::extract_locality(cve_edo = cve_edo,
                                     locality = locality) |>
        dplyr::select(-NOMGEO)
    
    # Step 3. extract the geocoded cases #####
    # Filtrar casos de forma segura
    z_filtered <- z |>
        dplyr::filter(ANO %in% c(yrs)) |>
        dplyr::mutate(x = long,
                      y = lat) |>
        # Eliminar puntos sin coordenadas válidas
        dplyr::filter(!is.na(x), !is.na(y), x != 0, y != 0)
    
    # Intersección espacial segura (con manejo de errores)
    if(nrow(z_filtered) > 0) {
        # Convertir a sf si no lo está
        if(!inherits(z_filtered, "sf")) {
            z_sf <- sf::st_as_sf(z_filtered, coords = c("x", "y"), crs = 4326, remove = FALSE)
        } else {
            z_sf <- z_filtered
        }
        
        # Realizar intersección espacial
        z_final <- tryCatch({
            z_sf[loc, ]
        }, error = function(e) {
            # Si falla la intersección, devolver vacío
            z_sf[0, ]
        })
        
        # Convertir a dataframe para el mapa (evitar problemas con geometrías)
        z_map <- z_final |>
            sf::st_drop_geometry() |>
            dplyr::filter(!is.na(x), !is.na(y))
        
        # Añadir columna 'mag' para el heatmap (por defecto = 1)
        if(nrow(z_map) > 0) {
            z_map$mag <- 1
        }
    } else {
        z_map <- data.frame(x = numeric(0), y = numeric(0), mag = numeric(0))
    }
    
    # Step 4. Generar ID único para el mapa
    if(is.null(unique_id)) {
        unique_id <- paste0("map_", cve_edo, "_", gsub(" ", "_", locality), "_", 
                            paste0(sample(c(letters, 0:9), 8, TRUE), collapse = ""))
    }
    
    # Step 5. Crear el mapa base
    mapa <- mapgl::maplibre(
        bounds = loc,
        color = NA,
        width = "100%",
        height = "400px",
        style = mapgl::carto_style(style_name = "positron")
    )
    
    # Añadir ID al elemento HTML del mapa (si es posible)
    mapa$x$elementId <- unique_id
    
    # Añadir fuentes y capas solo si hay datos
    mapa <- mapa |>
        mapgl::add_source("area", data = loc)
    
    # Añadir capas de área
    mapa <- mapa |>
        mapgl::add_fill_layer(id = paste0("ciudad-fill-", unique_id),
                              source = "area",
                              fill_color = "transparent",
                              fill_opacity = 0) |>
        mapgl::add_line_layer(id = paste0("ciudad-borde-", unique_id),
                              source = "area",
                              line_color = "#444444",
                              line_width = 1)
    
    # Añadir heatmap solo si hay casos
    if(nrow(z_map) > 0) {
        # Crear source con ID único para los casos
        source_id <- paste0("casos_", unique_id)
        mapa <- mapa |>
            mapgl::add_source(source_id, data = z_map) |>
            mapgl::add_heatmap_layer(id = paste0("dengue_cases_", unique_id),
                                     source = source_id,
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
                                     heatmap_opacity = 0.7)
    }
    
    # Añadir controles y forzar renderizado
    mapa <- mapa |>
        mapgl::add_fullscreen_control(position = "top-left") |> 
        mapgl::add_navigation_control() |>
        mapgl::add_scale_control()
    
    # Forzar renderizado en pestañas ocultas
    htmlwidgets::onRender(mapa, sprintf('
        function(el, x) {
            // Guardar el ID del mapa
            var mapId = "%s";
            
            // Función para forzar resize
            function forceResize() {
                if (el.map && typeof el.map.resize === "function") {
                    setTimeout(function() {
                        el.map.resize();
                    }, 100);
                }
            }
            
            // Observar cambios en pestañas
            var tabPane = el.closest(".tab-pane");
            if (tabPane) {
                var observer = new MutationObserver(function(mutations) {
                    if (tabPane.classList.contains("active")) {
                        forceResize();
                        observer.disconnect();
                    }
                });
                observer.observe(tabPane, {attributes: true});
            }
            
            // Forzar resize inicial después de un delay
            setTimeout(forceResize, 200);
        }
    ', unique_id))
    
    return(mapa)
}