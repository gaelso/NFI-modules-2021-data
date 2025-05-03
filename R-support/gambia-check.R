
## Preparation of tree location maps and plot positioning from GPS records
## Based on NFMA MS Access database

## 04-2025, Gaël Sola, FAO


##
## Intro ####
##

## This scripts originally runs as part of the NFI-modules-2021-data
## The first sections extract Gambia core data for the preparation of tree positioning maps 
## and plot re-positioning based on GPS records rather than the planned grid.

## If running the script directly with the country data made ready by Gaël
## - create a R project 
## - Within the project create folders:
##    - data
##    - data/Gambia
##    - results
##    - results/Gambia
## - Copy/paste the data in the folder "data/Gambia"
## - Copy/paste the R script in the R project main directory
## - Run the R script



##
## Setup ####
##

## Function top load package sand install them if missing
use_package <- function(.pkg_name) {
  pkg_name <- as.character(substitute(.pkg_name))
  if (!require(pkg_name, character.only = T,  quietly = TRUE)){
    install.packages(pkg_name, dep =TRUE)
    library(pkg_name, character.only = T, quietly = TRUE)
  } 
}

use_package(tidyverse)
use_package(sf)

## List for temporary objects
tmp <- list()

## List for country specific objects
ct <- list()

## Country to look for in global dataset
init <- list()
init$country_name <- "Gambia"
init$country_iso  <- "GMB"

## Create data from global analysis?
init$run_extraction <- FALSE

## Paths / Create dir
if (!init$country_name %in% list.dirs("data", full.names = F)) dir.create(paste0("data/", init$country_name))
if (!init$country_name %in% list.dirs("results", full.names = F)) dir.create(paste0("results/", init$country_name))



##
## Re-run the extraction part of the global analysis ####
##

if (init$run_extraction) {
  
  ## Run global NFMA data preparation
  source('R/00-functions-misc.R')
  source('R/00-functions-NFMA.R')
  source("R/00-paths.R")
  source('R/NFMA-01a-load-data.R')
  source('R/NFMA-01b-load-auxiliary.R')
  source('R/NFMA-02a-simplify-tract.R')
  source('R/NFMA-02b-simplify-plot.R')
  source('R/NFMA-02c-simplify-lus.R')
  source('R/NFMA-02d-simplify-tree.R')
  
  
  ct$tract <- tract02 |> filter(iso == init$country_iso)
  ct$plot  <- plot02  |> filter(iso == init$country_iso)
  ct$lus   <- lus02   |> filter(iso == init$country_iso)
  ct$tree  <- tree02  |> filter(iso == init$country_iso)
  
  ## Get GPS records and convert to 
  ct$plot_gps <- plot01 |> 
    filter(country == init$country_name) |>
    select(where(~ !all(is.na(.)))) |> 
    select(
      tract_id = ID.TRACT, 
      plot_id = ID.PLOT,
      plot_no = X3.PlotNo,
      plot_gps_x__leavingroad = X34g.PlotLeavingRoadX,
      plot_gps_y__leavingroad = X34h.PlotLeavingRoadY,
      plot_gps_x_startingpoint = X39a.UTME,
      plot_gps_y_startingpoint = X39b.UTMN,
      plot_gps_x_middlepoint = X39c.UTME,
      plot_gps_y_middlepoint = X39d.UTMN,
      plot_gps_x_endpoint = X39e.UTME,
      plot_gps_y_endpoint = X39f.UTMN
    ) |>
    mutate(crs = 32628)
  
  ## Save GMD full data
  walk(names(ct), function(x){ write_csv(ct[[x]], paste0("data/", init$country_name,"/", init$country_iso  , "-", x, ".csv")) })
  
  ## Remove all objects except 'init'
  rm(list = str_subset(ls(), pattern = "init", negate = T))
  
  ## recreate lists
  ct <- list()
  tmp <- list()

} 


##
## Country analysis ####
##

tmp$files <- list.files(file.path("data", init$country_name), pattern = "\\.csv", full.names = T)
tmp$names <- tmp$files |> str_remove(".*-") |> str_remove(".csv")
ct <- map(tmp$files, read_csv, show_col_types = F)
names(ct) <- tmp$names

## Convert GMP plot GPS points to long table
ct$plot_gpslatlon <- ct$plot_gps |>
  pivot_longer(
    cols = starts_with("plot_gps"),
    names_to = c(".value", "location"),
    names_pattern = "plot_gps_(.)_(.*)" 
    ) |>
  arrange(tract_id, plot_id)

## Convert UTM to latlon
ct$sf_plot_gps <- gmb_plot_gps124 |>
  filter(!is.na(x), !is.na(y)) |>
  mutate(xx = x, yy = y) |>
  st_as_sf(coords = c("xx", "yy"), crs = 32628) |>
  st_transform(crs = 4326)

gmb_plot_gps124_latlong <- sf_gmb_plot_gps124 |>
  mutate(
    lon = st_coordinates(sf_gmb_plot_gps124)[,1],
    lat = st_coordinates(sf_gmb_plot_gps124)[,2]
  ) |>
  as_tibble() |>
  select(-geometry)

ggplot(sf_gmb_plot_gps124) + 
  geom_sf(aes(color = location)) +
  theme_bw()

write_csv(gmb_plot_gps124_latlong, "tract124_GPS.csv")
st_write(sf_gmb_plot_gps124, "tract124_GPS.kml")


table(lus_gmb$lus)

tt <- lus_gmb |> summarise(count = n(), .by = c(iso, plot_id))

table(tt$count)

length(unique(lus_gmb$plot_id)) / 4


gmb_plot124 <- gmb_plot |> filter(tract_id == "GMB_661124")
gmb_lus124  <- gmb_lus |> filter(plot_id %in% c("GMB_6611241", "GMB_6611242", "GMB_6611243", "GMB_6611244"))
gmb_tree124 <- gmb_tree |> filter(plot_id %in% c("GMB_6611241", "GMB_6611242", "GMB_6611243", "GMB_6611244"))

table(gmb_tree124$plot_id)


write_csv(gmb_tree124, "gmb_tree124.csv")

gmb_tree124 |> 
  filter(plot_id == "GMB_6611241") |>
  ggplot(aes(tree_x, tree_y)) +
  geom_point(aes(color = tree_species_code)) +
  ggrepel::geom_text_repel(aes(label = tree_dbh)) +
  xlim(-10, 10) +
  theme_bw() + 
  coord_fixed() +
  labs(subtitle = "124/plot 1")


gmb_tree124 |> 
  filter(plot_id == "GMB_6611242") |>
  ggplot(aes(tree_x, tree_y)) +
  geom_point(aes(color = tree_species_code)) +
  ggrepel::geom_text_repel(aes(label = tree_dbh)) +
  xlim(-10, 10) +
  theme_bw() + 
  coord_fixed() +
  labs(subtitle = "124/plot 2")

gmb_tree124 |> 
  filter(plot_id == "GMB_6611243") |>
  ggplot(aes(tree_x, tree_y)) +
  geom_point(aes(color = tree_species_code)) +
  ggrepel::geom_text_repel(aes(label = tree_dbh)) +
  xlim(-10, 10) +
  theme_bw() + 
  coord_fixed() +
  labs(subtitle = "124/plot 3")

gmb_tree124 |> 
  filter(plot_id == "GMB_6611244") |>
  ggplot(aes(tree_x, tree_y)) +
  geom_point(aes(color = tree_species_code)) +
  ggrepel::geom_text_repel(aes(label = tree_dbh)) +
  xlim(-10, 10) +
  theme_bw() + 
  coord_fixed() +
  labs(subtitle = "124/plot 4")


