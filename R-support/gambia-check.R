
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
use_package(ggrepel)
use_package(ggpubr)

## List for temporary objects
tmp <- list()

## List for country specific objects
ct <- list()

## Country to look for in global dataset
init <- list()
init$country_name <- "Gambia"
init$country_iso  <- "GMB"

## Create data from global analysis?
init$run_extraction <- F

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
## Country analysis - GPS records ####
##

## Read country files 
tmp$files <- list.files(file.path("data", init$country_name), pattern = "\\.csv", full.names = T)
tmp$names <- tmp$files |> str_remove(".*-") |> str_remove(".csv")
ct <- map(tmp$files, read_csv, show_col_types = F)
names(ct) <- tmp$names

## Convert plot GPS points to long table
ct$plot_gps_records <- ct$plot_gps |>
  pivot_longer(
    cols = starts_with("plot_gps"),
    names_to = c(".value", "location"),
    names_pattern = "plot_gps_(.)_(.*)" 
    ) |>
  arrange(tract_id, plot_id)

## Convert UTM to latlon
ct$sf_plot_gps_records <- ct$plot_gps_records |>
  filter(!is.na(x), !is.na(y)) |>
  mutate(xx = x, yy = y) |>
  st_as_sf(coords = c("xx", "yy"), crs = 32628) |>
  st_transform(crs = 4326)

ct$plot_gps_records_latlon <- ct$sf_plot_gps_records |>
  mutate(
    lon = st_coordinates(ct$sf_plot_gps_records)[,1],
    lat = st_coordinates(ct$sf_plot_gps_records)[,2]
  ) |>
  as_tibble() |>
  select(-geometry)

## Check
ggplot(ct$sf_plot_gps_records) + 
  geom_sf(aes(color = location)) +
  theme_bw()

## Save data
write_csv(ct$sf_plot_gps_records, paste0("results/", init$country_name, "/", init$country_iso, "-plot-GPS-records.csv"))
st_write(ct$sf_plot_gps_records, paste0("results/", init$country_name, "/", init$country_iso, "-plot-GPS-records.kml"))


##
## Country analysis - tree position ####
##

## Checks
# table(ct$lus$lus)
# 
# tt <- ct$lus |> summarise(count = n(), .by = c(iso, plot_id))
# 
# table(tt$count)
# 
# length(unique(ct$lus$plot_id)) / 4

## For each cluster make tree location maps
tmp$list_tract <- ct$tree |>
  mutate(
    tract_id = str_sub(plot_id, end = -2)
  ) |>
  pull(tract_id) |>
  unique() |>
  sort()

tmp$gg_tracts <- map(tmp$list_tract, function(x){
  
  tract_id <- tmp$plot_no |> filter(tract_id == x) |> pull(tract_id) |> unique()
  
  ct$tree |> 
    filter(plot_id %in% paste0(x, 1:4)) |>
    mutate(
      plot_no = str_remove(plot_id, pattern = x),
      lus_id = paste0(plot_id, "0", lus_no)
    ) |>
    left_join(tmp$lus, by = join_by(lus_id)) |>
    ggplot(aes(tree_x, tree_y)) +
    geom_point(aes(color = tree_species_name, shape = lus)) +
    ggrepel::geom_text_repel(aes(label = paste0(tree_dbh, " cm")), min.segment.length = 0, size = 2) +
    theme_bw() + 
    coord_fixed(ratio = 1/2) +
    facet_wrap(~plot_no, nrow = 1) + 
    scale_y_continuous(breaks = 1:25 * 10) +
    scale_x_continuous(breaks = c(-10, 0, 10), minor_breaks = c(-5, 0, 5), limits = c(-10, 10)) +
    labs(
      subtitle = paste0("Tree DBH for Tract ", tract_id),
      color = "",
      shape = "",
      x = "X (m)",
      y = "Y (m)",
      caption = "Plot ratio y/x = 1/2"
    )
  
})

names(tmp$gg_tracts) <- tmp$list_tract

## Write all plots
walk(tmp$list_tract, function(x){
  
  ggsave(
    plot = tmp$gg_tracts[[x]], 
    filename = paste0("results/", init$country_name, "/", init$country_iso, "-tree-position-tract0-", x, ".png"),
    height = 17, width = 14, units = "cm", dpi = 300
    )
  
})


