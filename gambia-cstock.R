
library(sf)
library(terra)
library(geosphere)
library(tidyterra)
library(tidyverse)


##
## LOAD DATA ####
##

## + load NFMA tree data ####

path_nfma       <- list.files("data/Gambia", pattern = "GMB-.*\\.csv", full.names = T)
path_nfma_names <- list.files("data/Gambia", pattern = "GMB-.*\\.csv") |> str_remove("GMB-") |> 
  str_remove("\\.csv")

nfma <- map(path_nfma, read_csv, show_col_types = F)
names(nfma) <- path_nfma_names

walk(path_nfma_names, function(x){
  assign(paste0(x, "_init"), nfma[[x]], envir = globalenv())
})

## + get Chave 2014 E

if (!"E.bil" %in% list.files("data/Gambia")) {
  download.file(
    url = "https://github.com/umr-amap/BIOMASS/raw/refs/heads/master/data-raw/climate_variable/E.zip",
    destfile = file.path("data/Gambia", "E.zip")
  )
  unzip(
    zipfile = file.path("data/Gambia", "E.zip"), exdir = "data/Gambia"
  )
}

rs_E <- terra::rast("data/Gambia/E.bil")

#plot(rs_E)

## + Get WD at species level ####

if (!"wdData.csv" %in% list.files("data/Gambia")) {
  download.file(
    url = "https://raw.githubusercontent.com/umr-amap/BIOMASS/refs/heads/master/data-raw/wdData.csv",
    destfile = file.path("data/Gambia", "wdData.csv")
  )
}

wd_init <- read_csv("data/Gambia/wdData.csv", show_col_types = F) |>
  select(wd_no = Number, wd_family = Family, wd_species_name = Binomial, wd_gcm3 = `Wood density (g/cm^3), oven dry mass/fresh volume`, wd_region = Region)

table(wd_init$Region)



## 
## preliminary calculations ####
##

## + make species WD averages for Africa ####
wd_sp <- wd_init |> 
  filter(wd_region == "Africa (tropical)") |>
  group_by(wd_species_name) |>
  summarise(
    count_avg_species = n(),
    wd_avg_species = mean(wd_gcm3), 
    wd_std_species = sd(wd_gcm3),
    .groups = "drop"
    )


## + Get land use section areas
lus <- lus_init |>
  mutate(area_ha = round(plot_length * plot_width / 10000, 3)) |>
  select(tract_no, plot_no, lus_no, lus, lus_class, area_ha)

## Get E at plot level
sf_plot_gps <- plot_gps_init |>
  mutate(
    tract_no = as.numeric(str_sub(tract_id, -3)),
    plot_no  = as.numeric(str_sub(plot_id, -1)),
    x = plot_gps_x_middlepoint, 
    y = plot_gps_y_middlepoint
    ) |>
  select(tract_no, plot_no, x, y) |>
  filter(!is.na(x), !is.na(y)) |>
  st_as_sf(coords = c("x", "y"), crs = 32628) |>
  st_transform(crs = 4326)

tmp_E <- terra::extract(rs_E, vect(sf_plot_gps))

# ggplot() +
#   geom_spatraster(data = rs_E) +
#   geom_sf(data = sf_plot_gps) +
#   coord_sf(
#     xlim = st_bbox(sf_plot_gps)[c("xmin", "xmax")],
#     ylim = st_bbox(sf_plot_gps)[c("ymin", "ymax")]
#     )

plot_E <- sf_plot_gps |>
  bind_cols(E = tmp_E[,2]) |>
  as_tibble() |>
  select(-geometry)


## Get LUS and E at tree level ####

tree <- tree_init |>
  mutate(
    tract_no = as.numeric(str_sub(plot_id, 8, 10)),
    plot_no = as.numeric(str_sub(plot_id, start = -1))
    ) |>
  select(country, iso, tract_no, plot_no, lus_no, everything()) |>
  left_join(lus, by = join_by(tract_no, plot_no, lus_no)) |>
  left_join(plot_E, by = join_by(tract_no, plot_no)) |>
  mutate(
    tree_h = exp(-1.803 -0.976 * E + 0.976 * log(tree_wd) + 2.673 * log(tree_dbh) - 0.0299 * (log(tree_dbh))^2)
  )


