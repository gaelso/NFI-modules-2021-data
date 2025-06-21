
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

## Get LUS at tree level ####

tree <- tree_init |>
  mutate(lus_id = paste0(plot_id, lus_no)) |>
  left_join(lus_init, by = join_by(country, iso, plot_id, lus_no, lus_id))



