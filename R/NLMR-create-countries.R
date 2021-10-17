## NFI-modules-2021-learnr
## Gael Sola, FAO


## Create countries #########################################################

##
## Louland ##################################################################
##

dir.create("results/louland", showWarnings = FALSE)

## Params
nb_ft <- 4
mg    <- T
alt   <- 2000
sea   <-  0.2


## Create raster files
louland <- create_newland(.seed = 11, .alt = alt, .sea = sea, .nb_ft = nb_ft, .mg = mg, .river = F)

writeRaster(louland$topo    , "results/louland/topo.tiff"    , overwrite=T)
writeRaster(louland$topo_map, "results/louland/topo_map.tiff", overwrite=T)
writeRaster(louland$lc      , "results/louland/lc.tiff"      , overwrite=T)
writeRaster(louland$lc_map  , "results/louland/lc_map.tiff"  , overwrite=T)

## Create image
png(filename = paste0("results/louland/louland.png"), width = 800, height = 800)
make_3d(.country = louland)
render_snapshot()
dev.off()

## Create video
render_movie(filename = paste0("results/louland/louland"))
rgl::rgl.close()


## Create land cover shapefile
sf_louland_lc <- st_as_stars(deratify(louland$lc)) %>%
  st_as_sf(., as_points = FALSE, merge = TRUE) %>%
  st_make_valid() %>%
  mutate(id = 1:nrow(.))

## Create country boundaries
sf_louland_lc %>% 
  filter(lc == "WA") %>% 
  ggplot() +
  geom_sf(aes(fill = as.character(id)))

sf_louland <- sf_louland_lc %>%
  filter(lc != "WA" | id == 145) %>% 
  st_union()

## Write shapefile
dir.create('results/louland/lc', showWarnings = F)
dir.create('results/louland/adm', showWarnings = F)
st_write(sf_louland_lc, "results/louland/lc/lc.shp", delete_dsn = T)
st_write(sf_louland, "results/louland/adm/adm.shp", delete_dsn = T)

ggplot() +
  geom_sf(data = sf_louland_lc, aes(fill = lc)) +
  scale_fill_manual(values = c("#73c2fb", "#edf5e1", "#8ee4af", "#5cdb95", "#379683", "#00743f", "#012172")) +
  geom_sf(data = sf_louland, fill = NA, size = 1.5, color = "red")


## Mapping 
# pal <- c("#73c2fb", "#edf5e1", "#8ee4af", "#5cdb95", "#379683", "#00743f", "#012172")
# pal2 <- c("#edf5e1", "#8ee4af", "#5cdb95", "#379683", "#00743f", "#012172")
# 
# ggplot() +
#   geom_sf(data = sf_louland_lc, aes(fill = lc), col = NA) +
#   scale_fill_manual(values = pal) +
#   geom_sf(data = sf_louland, fill = NA, col = "black", size = 1) +
#   theme_void()
# 
# tmap_mode("view")
# tm_shape(sf_louland_lc %>% filter(lc != "WA")) + tm_polygons(col = "lc", palette = pal2, border.col = NULL) +
# tm_shape(sf_louland) + tm_borders(lwd = 2)



## Save core graphs for different seeds with 3D snapshot ####################

# dir.create("results/check", showWarnings = F)
# dir.create("results/check_vid", showWarnings = F)
# #dir.create("results/check_gif", showWarnings = F)
# 
# for (i in 11) {
#   tmp <- make_country(.seed = i, .tmp = T, .river = F)
#   png(filename = paste0("results/check/lc", i, "_v2.png"), width = 1200, height = 1200)
#   par(mfrow = c(2, 2))
#   plot(tmp$lc, main = paste0("Land cover, seed: ", i), col = tmp$param$hex, legend = FALSE, axes = F)
#   legend("bottomleft", 
#          legend = tmp$param$lc, 
#          fill = tmp$param$hex) 
#   plot(tmp$topo, main = paste0("Elevation, seed: ", i), axes = F)
#   hist(tmp$lc)
#   make_3d(.country = tmp)
#   render_snapshot()
#   par(mfrow = c(1, 1))
#   dev.off()
#   render_movie(filename = paste0("results/check_vid/lc", i, "_v2"))
#   render_movie(filename = paste0("results/check_vid/lc", i, ".gif"))
#   
# }
# 
# rgl::rgl.close()


