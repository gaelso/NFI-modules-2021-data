
library(Hmisc)
library(sf)
library(tmap)
library(geodata)
library(tidyverse)


source('R/00-functions-NFMA.R')

## 
## Get back to MDB file to extract planned locations ####
##

path_mdb <- list.files(path = "data/NFMA/mdb_files", pattern = ".*(NFA|NFI|NFMA|ILUA)", full.names = TRUE, recursive = TRUE)
path <- stringr::str_subset(path_mdb, pattern = "Gambia|GAMBIA")

mdb.get(path[2], tables = T)


## GET TRACT DATA ####
tract_init <- mdb.get(path[2], "F1-Tract") |> rm_labels() |> as_tibble()
# names(tract_init)

## Make spatial object
sf_tract <- tract_init |>
  select(
    ID.TRACT, 
    nfma_tc_lat = X12a.TractCenterLatitude, 
    nfma_tc_lon = X12b.TractCenterLongitude,
  ) |>
  mutate(
    nfma_tract_id = paste0("t", str_sub(ID.TRACT, start = 4)),
    xx = nfma_tc_lon, 
    yy = nfma_tc_lat
  ) |>
  select(nfma_tract_id, nfma_tc_lon, nfma_tc_lat, xx, yy) |>
  st_as_sf(coords = c("xx", "yy"), crs = 4326)


## REMOVE TRACT OUTSIDE BOUNDARIES FROM REPORT
sf_tract <- sf_tract |>
  filter(!nfma_tract_id %in% paste0("t", c(141, 106, 113, 114, 103, 105))) |>
  filter(!nfma_tract_id %in% paste0("t0", c(75, 76, 11, 87, 91, 98, 48, 51, 52, 71)))

##
## Get initial 5 arcminute grid and other spatial data
##

sf_grid_init <- st_read("data/Gambia/Gambia_SU.kml")
## Replaced with Iveren data
#sf_country <- geodata::gadm("Gambia", level = 0, path = "data/Gambia") |> st_as_sf()
sf_country <- st_read("data/Gambia/Admin boundries/LG_Areas.shp") |> 
  st_transform(4326)
sf_country_buff <- sf_country |> st_buffer(dist = 200)


## Starting point from initial grid
start_point <- sf_grid_init |> 
  filter(Name == 149) %>%
  mutate(geometry = st_geometry(.) + c(-5/60, 0))

sf_grid25 <- st_make_grid(
  sf_country,
  cellsize = 2.5/60, 
  offset = st_coordinates(start_point), 
  square = T,
  what = "corners"
) |>
  st_as_sf() |>
  rename(geometry = x) |>
  st_filter(sf_country_buff) |>
  mutate(
    cluster_no = row_number(),
    cluster_id = case_when(
      cluster_no < 10 ~ paste0("c00", cluster_no),
      cluster_no < 100 ~ paste0("c0", cluster_no),
      TRUE ~ paste0("c", cluster_no)
    )
  )

# CHECK
# tmap_mode("view") +
#   tm_shape(sf_tract) + tm_dots(size = 0.6) +
#   tm_shape(sf_grid_init) + tm_dots(fill = "lightgreen", size = 0.6) +
#   tm_shape(sf_grid25) + tm_dots(fill = "pink3", size = 0.4, col = "black", lwd = 0.1) +
#   tm_shape(sf_country) + tm_lines()

## Add tract info to 2.5 arcmin grid
sf_tract_buff <- st_buffer(sf_tract, dist = 500)
sf_grid25_join <- st_join(sf_grid25, sf_tract_buff)


##
## Convert 2.5 arcminutes grid to UTM to get NFMA plot 1 converted to cross
##

## Get grid center points matching tract center location
grid25_m <- sf_grid25_join %>%
  mutate(
    tc_no = row_number(),
    tc_id = case_when(
      tc_no < 10 ~ paste0("c00", tc_no),
      tc_no < 100 ~ paste0("c0", tc_no),
      TRUE ~ paste0("c", tc_no)
    ),
    tc_lon = st_coordinates(.)[,1],
    tc_lat = st_coordinates(.)[,2],
  ) |>
  st_transform(crs = 32628) %>%
  mutate(
    tc_x = st_coordinates(.)[,1],
    tc_y = st_coordinates(.)[,2]
  ) |>
  as_tibble() |>
  select(-geometry) 


## Make the cross locations
ceo_grid25 <- grid25_m |>
  mutate(
    x_plot1 = tc_x - 250,
    y_plot1 = tc_y - 125,
    x_plot2 = x_plot1,
    y_plot2 = y_plot1 + 100,
    x_plot3 = x_plot1 + 100,
    y_plot3 = y_plot1,
    x_plot4 = x_plot1,
    y_plot4 = y_plot1 - 100,
    x_plot5 = x_plot1 - 100,
    y_plot5 = y_plot1,
  ) |> 
  select(cluster_no, cluster_id, nfma_tract_id, starts_with(c("x_", "y_"))) |>
  pivot_longer(cols = starts_with(c("x_", "y_")), names_to = c("xy", "plot_no"), names_pattern = "(.)_plot(.)", values_to = "coord") |>
  pivot_wider(names_from = xy, values_from = coord) |>
  mutate(
    plot_no = as.numeric(plot_no),
    plot_id = paste0(cluster_id, plot_no)
  ) |>
  select(cluster_no, cluster_id, nfma_tract_id, plot_no, plot_id, plot_x = x, plot_y = y)

## Convert back to lat/lon
sf_ceo_grid25 <- ceo_grid25 |>
  mutate(xx = plot_x, yy = plot_y) |>
  st_as_sf(coords = c("xx", "yy"), crs = 32628) |>
  st_transform(crs = 4326)

## Check
# tmap_mode("view") +
#   tm_shape(sf_country) + tm_lines() +
#   tm_shape(sf_tract) + tm_dots(size = 0.6) +
#   tm_shape(sf_ceo_grid25) + tm_dots(fill = "green", size = 0.4)


## Make table
ceo25_latlon <- sf_ceo_grid25 |>
  mutate(
    plot_lon = st_coordinates(sf_ceo_grid25)[,1],
    plot_lat = st_coordinates(sf_ceo_grid25)[,2]
  ) |>
  as_tibble() |>
  select(-geometry)

## Final tables
ceo25_intens <- ceo25_latlon |>
  filter(is.na(nfma_tract_id))

ceo5_corr <- ceo25_latlon |>
  filter(!is.na(nfma_tract_id))


##
## Convert NFMA tract center to cross points
##

ceo_grid_nfma <- sf_tract |>
  st_transform(32628) %>%
  mutate(
    tc_x = st_coordinates(.)[,1],
    tc_y = st_coordinates(.)[,2]
  ) |>
  as_tibble() |>
  select(-geometry) |>
  mutate(
    x_plot1 = tc_x - 250,
    y_plot1 = tc_y - 125,
    x_plot2 = x_plot1,
    y_plot2 = y_plot1 + 100,
    x_plot3 = x_plot1 + 100,
    y_plot3 = y_plot1,
    x_plot4 = x_plot1,
    y_plot4 = y_plot1 - 100,
    x_plot5 = x_plot1 - 100,
    y_plot5 = y_plot1,
  ) |> 
  select(nfma_tract_id, starts_with(c("x_", "y_"))) |>
  pivot_longer(cols = starts_with(c("x_", "y_")), names_to = c("xy", "plot_no"), names_pattern = "(.)_plot(.)", values_to = "coord") |>
  pivot_wider(names_from = xy, values_from = coord) |>
  mutate(
    plot_no = as.numeric(plot_no),
    plot_id = paste0(nfma_tract_id, plot_no)
  ) |>
  select(nfma_tract_id, plot_no, plot_id, plot_x = x, plot_y = y)

## Convert back to lat/lon
sf_ceo_grid_nfma <- ceo_grid_nfma |>
  mutate(xx = plot_x, yy = plot_y) |>
  st_as_sf(coords = c("xx", "yy"), crs = 32628) |>
  st_transform(4326) 

## make table
ceo5_nfma <- sf_ceo_grid_nfma |>
  mutate(
    plot_lon = st_coordinates(sf_ceo_grid_nfma)[,1],
    plot_lat = st_coordinates(sf_ceo_grid_nfma)[,2]
  ) |>
  as_tibble() |>
  select(-geometry)

## Check
sf_ceo5_nfma <- ceo5_nfma |>
  mutate(xx = plot_lon, yy = plot_lat) |>
  st_as_sf(coords = c("xx", "yy"), crs = 4326)

sf_ceo25_intens <- ceo25_intens |>
  mutate(xx = plot_lon, yy = plot_lat) |>
  st_as_sf(coords = c("xx", "yy"), crs = 4326)

sf_ceo5_corr <- ceo5_corr |>
  mutate(xx = plot_lon, yy = plot_lat) |>
  st_as_sf(coords = c("xx", "yy"), crs = 4326)

tmap_mode("view") +
  tm_shape(sf_tract) + tm_dots(size = 0.6) +
  tm_shape(sf_grid25) + tm_dots(size = 0.6, fill = "grey40") +
  tm_shape(sf_ceo5_nfma) + tm_dots(fill = "red", size = 0.4) +
  tm_shape(sf_ceo5_corr) + tm_dots(fill = "pink", size = 0.4) +
  tm_shape(sf_ceo25_intens) + tm_dots(fill = "green", size = 0.4)



##
## Write results
##

if(!"CEO" %in% list.dirs("results/Gambia", full.names = F)) dir.create("results/Gambia/CEO")

write_csv(ceo5_nfma, "results/Gambia/CEO/ceo5_NFMA.csv")
write_csv(ceo5_corr, "results/Gambia/CEO/ceo5_grid.csv")
write_csv(ceo25_intens, "results/Gambia/CEO/ceo25_intens.csv")

st_write(sf_ceo5_nfma, "results/Gambia/CEO/ceo5_NFMA.kml")
st_write(sf_ceo5_corr, "results/Gambia/CEO/ceo5_grid.kml")
st_write(sf_ceo25_intens, "results/Gambia/CEO/ceo25_intens.kml")


## stats
nrow(ceo5_nfma)/5
nrow(ceo25_intens)/5

nrow(ceo5_nfma) + nrow(ceo25_intens)
nrow(ceo5_nfma) + nrow(ceo25_intens) + nrow(ceo5_corr)
