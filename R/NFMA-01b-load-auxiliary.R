## NFI-modules-2021-learnr
## Gael Sola, FAO

##
## Spatial data #############################################################
##

## --- World countries ------------------------------------------------------

if (!("gadm36_levels_shp" %in% list.files("data/GIS"))) {
  utils::download.file(
    url      = "https://biogeo.ucdavis.edu/data/gadm3.6/gadm36_levels_shp.zip", 
    destfile = "data/GIS/gadm36_levels_shp.zip"
  )
  utils::unzip(
    zipfile = "data/GIS/gadm36_levels_shp.zip",
    files   = c("gadm36_0.cpg", "gadm36_0.dbf", "gadm36_0.prj", "gadm36_0.shp", "gadm36_0.shx"), 
    exdir   = "data/GIS/gadm36_levels_shp"
  )
  unlink("data/GIS/gadm36_levels_shp.zip")
}

sf_country <- st_read("data/GIS/gadm36_levels_shp/gadm36_0.shp", quiet = TRUE)



## --- FAO Global Ecological Zones ------------------------------------------

if (!("gez2010" %in% list.files("data/GIS"))) {
  utils::download.file(
    url      = "https://storage.googleapis.com/fao-maps-catalog-data/uuid/2fb209d0-fd34-4e5e-a3d8-a13c241eb61b/resources/gez2010.zip", 
    destfile = "data/GIS/gez2010.zip"
  )
  utils::unzip(
    zipfile = "data/GIS/gez2010.zip",
    #files   = "E.nc", 
    exdir   = "data/GIS/gez2010"
  )
  unlink("data/GIS/gez2010.zip")
}

if (!("gez_2010_wgs84.gpkg") %in% list.files("data/GIS")) {
  
  message("making GEZ shapefile valid...")
  sf_gez  <- st_read("data/GIS/gez2010/gez_2010_wgs84.shp", quiet = TRUE)
  sf_gez2 <- st_make_valid(sf_gez)
  
  write_sf(sf_gez2, "data/GIS/gez_2010_wgs84.gpkg")
  rm(sf_gez, sf_gez2)
  
  message("...Done")

}

sf_gez2 <- st_read("data/GIS/gez_2010_wgs84.gpkg", quiet = TRUE)


## --- Add Chave et al. 2014 Environment variable ---------------------------

## Load E raster file Download E.nc from: 
## http://chave.ups-tlse.fr/pantropical_allometry/E.nc.zip

if (!("E.nc" %in% list.files("data/GIS"))) {
  utils::download.file(
    url      = "http://chave.ups-tlse.fr/pantropical_allometry/E.nc.zip", 
    destfile = "data/GIS/E.nc.zip"
  )
  utils::unzip(
    zipfile = "data/GIS/E.nc.zip",
    files   = "E.nc", 
    exdir   = "data/GIS"
  )
  unlink("data/GIS/E.nc.zip")
}

envir_stress <- terra::rast("data/GIS/E.nc")
# plot(envir_stress)
# freq(envir_stress)
# raster::cellStats(envir_stress, stat = "min")

##
## Wood density #############################################################
##

## Download data if necessary (!!! Unofficial repo)
if (!("Cirad-wood-density-database.csv" %in% list.files("data/WD"))) {
  utils::download.file(
    url      = "https://raw.githubusercontent.com/ghislainv/wood-density-Cirad/master/Cirad-wood-density-database.csv", 
    destfile = "data/WD/Cirad-wood-density-database.csv"
  )
}

if (!("GlobalWoodDensityDatabase.txt" %in% list.files("data/WD"))) {
  utils::download.file(
    url      = "https://raw.githubusercontent.com/ghislainv/wood-density-Cirad/master/data/wsg/Dryad.wsg/GlobalWoodDensityDatabase.txt", 
    destfile = "data/WD/GlobalWoodDensityDatabase.txt"
  )
}


## Load data
cwdd <- readr::read_csv("data/WD/Cirad-wood-density-database.csv", show_col_types = F) |>
  dplyr::select(species_name = Taxa, continent = Continent, wd = Db) |>
  mutate(source = "CWDD")

gwd  <- readr::read_tsv("data/WD/GlobalWoodDensityDatabase.txt", show_col_types = F) |>
  dplyr::select(species_name = Binomial, continent = Region, wd = WSG) |>
  mutate(source = "GWD")

