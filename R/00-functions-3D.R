## NFI-modules-2021-learnr
## Gael Sola, FAO


## !!! For testing
# .country <- make_country(.seed = 11, .tmp = T)
## !!!


make_3d <- function(.country){
  
  lc_param <- .country$param %>% 
    bind_cols(as_tibble(t(col2rgb(.$hex)), .name_repair = ~c("r", "g", "b")))
  
  rr <- tibble(id = as.vector(flip(.country$lc_map))) %>%
    left_join(lc_param %>% dplyr::select(id, r), by = "id") %>%
    pull(r) %>%
    matrix(., ncol = ncol(.country$lc_map))
  
  gg <- tibble(id = as.vector(flip(.country$lc_map))) %>%
    left_join(lc_param %>% dplyr::select(id, g), by = "id") %>%
    pull(g) %>%
    matrix(., ncol = ncol(.country$lc_map))
  
  bb <- tibble(id = as.vector(flip(.country$lc_map))) %>%
    left_join(lc_param %>% dplyr::select(id, b), by = "id") %>%
    pull(b) %>%
    matrix(., ncol = ncol(.country$lc_map))
  
  rgb_array <- array(0, dim = c(nrow(.country$lc_map), ncol(.country$lc_map), 3))
  rgb_array[,,1] <- rr/255
  rgb_array[,,2] <- gg/255
  rgb_array[,,3] <- bb/255
  rgb_array <- aperm(rgb_array, c(1,2,3))
  
  ray    <- raster::as.matrix(flip(.country$topo_map))
  #shadow <- ray_shade(ray, zscale=1, lambert=FALSE)
  #amb    <- ambient_shade(ray, zscale=1, sunbreaks = 15, maxsearch = 100)
  
  ## Close opened rgl device
  if (length(rgl::rgl.dev.list()) != 0) rgl::rgl.close() 
  
  t1 <- Sys.time()
  ray %>%
    sphere_shade(texture = "imhof1") %>%
    #add_water(detect_water(ray, min_area = 100)) %>%
    add_overlay(rgb_array) %>%
    #add_shadow(ray_shade(ray, zscale=1, lambert=FALSE), 0.7) %>%
    #add_shadow(ambient_shade(ray, zscale=1, sunbreaks = 15, maxsearch = 100)) %>%
    add_shadow(lamb_shade(ray)) %>%
    plot_3d(
      ray,
      zscale=10, fov=0, theta=-45, phi=45, windowsize=c(250,250), zoom=0.8,
      water=TRUE, wateralpha = 0.8, watercolor = "#73c2fb", waterlinecolor = "white",
      waterlinealpha = 0.3, solid = FALSE
    )
  t2 <- Sys.time()
  dt    <- round(as.numeric(t2-t1, units = "secs"))
  message(paste0("Made 3D render of the new land in ", dt, " sec."))
  message(" ")
  
}
