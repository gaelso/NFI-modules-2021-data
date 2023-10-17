## NFI-modules-2021-learnr
## Gael Sola, FAO


## libs
#remotes::install_github("ropensci/NLMR")
#remotes::install_version("NLMR", "0.4")
#remotes::install_github("tylermorganwall/rayshader")
library(NLMR)
library(landscapetools)
library(raster)
# library(igraph)
# library(RSAGA)
library(terra)
library(sf)
# library(stars)
# library(rayshader)
library(rgl)
library(plotly)
library(scales)
library(smoothr)

## Tidy
library(ggpubr)
library(ggrepel)
library(stringi)
library(tidyverse)

options(dplyr.summarise.inform = FALSE)


source("R-NLMR/00-functions-NLMR.R")


dir.create("results/mock-country", showWarnings = FALSE)


## Create countries #########################################################



## Params
nb_ft <- 4
mg    <- T
alt   <- 2000
sea   <-  0.2


## Create raster files
mock_country <- create_newland(.seed = 11, .alt = alt, .sea = sea, .nb_ft = nb_ft, .mg = mg, .river = F)

mocks <- map(1:10, function(x){
  
  mock_country <- create_newland(.seed = x, .alt = alt, .sea = sea, .nb_ft = nb_ft, .mg = mg, .river = F)
  print(mock_country$lc_map)
  
  mock_country
  
})

for (i in 1:10) {
  plot(mocks[[i]]$lc_map)
}
plot(mocks[[2]]$lc_map)
plot(mocks[[3]]$lc_map)

mocks[[3]]
mocks[[9]]
mock_country$gr

mockss <- create_newland(.seed = 3, .alt = alt, .sea = sea, .nb_ft = nb_ft, .mg = mg, .river = F)
mockss


## Write raster data 
writeRaster(louland$topo    , "results/louland/topo.tiff"    , overwrite=T)
writeRaster(louland$topo_map, "results/louland/topo_map.tiff", overwrite=T)
writeRaster(louland$lc      , "results/louland/lc.tiff"      , overwrite=T)
writeRaster(louland$lc_map  , "results/louland/lc_map.tiff"  , overwrite=T)

## Create image
png(filename = paste0("results/louland/newland.png"), width = 250, height = 250)
make_3d(.country = louland)
render_snapshot()
dev.off()

## Create video
render_movie(filename = paste0("results/louland/newland.gif"))
rgl::rgl.close()

## Recreate parameters
louland_param <- tibble(
  lc_id = c(   5,    4,    3,    2),
  lc    = c("EV", "MD", "DD", "WL"),
  w     = c(0.11, 0.23, 0.08, 0.21)
) %>%
  bind_rows(list(lc_id = 1, lc = "NF", w = 1 - sum(.$w))) %>%
  mutate(hex = c("#00743f", "#379683", "#5cdb95", "#edeae5", "#f3e0dc")) %>%  ## WL: "#edf5e1"
  bind_rows(list(lc_id = c(0, 6), lc = c("WA", "MG"), w = c(0, 0), hex = c("#73c2fb", "#012172"))) %>%
  arrange(lc_id) %>%
  bind_cols(lc_name = c("Water", "Non-forest", "Other woodland", "Deciduous", "Mixed Deciduous", "Evergreen", "Mangrove"))

## Create land cover shapefile
sf_lc <- st_as_stars(deratify(louland$lc)) %>%
  st_as_sf(., as_points = FALSE, merge = TRUE) %>%
  st_set_crs(st_crs(32727)) %>%
  mutate(id = 1:nrow(.)) %>%
  st_cast("MULTIPOLYGON") %>%
  st_make_valid() %>%
  filter(lc != "WA" | id == 145)

table(st_is_valid(sf_lc))
st_is_valid(sf_lc, reason = TRUE)

ggplot() +
  geom_sf(data = sf_lc, aes(fill = lc))

sf_lc3 <- sf_lc %>%
  smoothr::smooth(method = "ksmooth", smoothness = 2.2) %>%
  mutate(id = 1:nrow(.))

table(st_is_valid(sf_lc3))
st_is_valid(sf_lc3, reason = TRUE)
 
sf_lc4 <- sf_lc3 %>%
  st_make_valid() %>%
  st_cast("POLYGON")

table(st_is_valid(sf_lc4))
st_is_valid(sf_lc4, reason = TRUE)

sf_lc5 <- sf_lc4 %>%
  left_join(louland_param %>% select(lc_id, lc, lc_name), by = "lc") %>% 
  mutate(lc = forcats::fct_reorder(lc, lc_id)) %>%
  select(id, lc_id, lc, lc_name)


## Create country boundaries
sf_admin <- sf_lc5 %>%
  st_buffer(100) %>%
  st_union(by_feature = FALSE)

st_is_valid(sf_admin)

## Create topo lines
topo_seq <- seq(from = round(minValue(louland$topo)), to = round(maxValue(louland$topo)), by = 100)
sf_topo <- louland$topo %>% st_as_stars() %>% st_contour(breaks = topo_seq)

## Write shapefile
dir.create('results/louland/lc', showWarnings = F)
dir.create('results/louland/adm', showWarnings = F)
dir.create('results/louland/topo', showWarnings = F)
st_write(sf_lc5, "results/louland/lc/lc.shp", delete_dsn = T)
st_write(sf_admin, "results/louland/adm/adm.shp", delete_dsn = T)
st_write(sf_topo, "results/louland/topo/topo.shp", delete_dsn = T)


## Mapping 
pal <- c("#73c2fb", "#edf5e1", "#ffcb9a", "#5cdb95", "#379683", "#00743f", "#012172") ## WL: "#edeae5", WA: "#73c2fb"


gr_map <- ggplot() +
  geom_sf(data = sf_lc, aes(fill = lc), col= NA) +
  scale_fill_manual(values = pal) +
  geom_sf(data = sf_admin, fill = NA, size = 0.6, color = "black") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "#73c2fb"),
    text = element_text(family = "LoraIt")
    ) +
  labs(fill = "") +
  coord_sf(xlim = c(-20.5, -19.5), ylim = c(-0.8, 0.2), expand = FALSE, crs = st_crs(4326)) +
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white"),
    text_family = "LoraIt"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr", 
    which_north = "true",
    pad_x = unit(0.2, "in"), 
    pad_y = unit(0.3, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "LoraIt"
    )
  )
gr_map
ggsave(plot = gr_map, filename = "results/louland/lc.png", width = 20, height = 15, dpi = 100, units = "cm")

gr_map2 <- gr_map + theme(legend.position = "none")
gr_map2
ggsave(plot = gr_map2, filename = "results/louland/lc-image.png", width = 15, height = 15, dpi = 100, units = "cm")


## Create a circle around the map
sf_circle <- st_centroid(sf_admin) %>%
  st_buffer(dist = 70000)

gr_icon <- ggplot() +
  geom_sf(data = sf_circle, fill = "#73c2fb", col = NA) +
  geom_sf(data = sf_lc, aes(fill = lc), col= NA) +
  scale_fill_manual(values = pal) +
  #geom_sf(data = sf_admin, fill = NA, size = 0.6, color = "black") +
  theme_void() +
  theme(legend.position = "none") +
  labs(fill = "") #+
  #coord_sf(xlim = c(-20.5, -19.5), ylim = c(-0.8, 0.2), expand = FALSE, crs = st_crs(4326))
gr_icon
ggsave(plot = gr_icon, filename = "results/louland/lc-icon.png", width = 1000, height = 1000, dpi = 300, units = "px")


sf_circle <- st_centroid(sf_admin) %>%
  st_buffer(dist = 200000)

gr_logo <- ggplot() +
  #geom_sf(data = sf_circle, fill = "#73c2fb", col = NA) +
  geom_sf(data = sf_lc, aes(fill = lc), col= NA) +
  scale_fill_manual(values = pal) +
  #geom_sf(data = sf_admin, fill = NA, size = 0.6, color = "black") +
  theme_void() +
  theme(legend.position = "none") +
  labs(fill = "") #+
#coord_sf(xlim = c(-20.5, -19.5), ylim = c(-0.8, 0.2), expand = FALSE, crs = st_crs(4326))
gr_logo
ggsave(plot = gr_logo, filename = "results/louland/lc-logo.png", width = 1000, height = 1000, dpi = 300, units = "px")


# tmap_mode("view")
# tmap_options(check.and.fix = TRUE)
# tm_shape(sf_lc5) + tm_polygons(col = "lc", palette = pal, border.col = NULL) +
# tm_shape(sf_admin) + tm_borders(lwd = 2)



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


