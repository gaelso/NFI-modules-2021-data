## NFI-modules-2021-learnr
## Gael Sola, FAO


## Create mock land and visualizations
#remotes::install_github("ropensci/NLMR")
#remotes::install_version("NLMR", "0.4")
#remotes::install_github("tylermorganwall/rayshader")
# library(NLMR)
# library(landscapetools)
# library(raster)
# library(igraph)
# library(RSAGA)
library(terra)
library(sf)
# library(stars)
# library(rayshader)
# library(rgl)
# library(plotly)
# library(scales)
# library(smoothr)
## Comment GS 2023/10/15: Removed all libs related to created mock country
library(ggspatial)

## Tidy
library(ggpubr)
library(ggrepel)
library(tmap)
library(stringi)
library(tidyverse)

options(dplyr.summarise.inform = FALSE)

## Comment GS 2023/10/15: Removed all packages not related to NFMA data analysis
# ## Fonts
# # remotes::install_version("Rttf2pt1", version = "1.3.8") ## Run 1 time
# library(extrafont)
# 
# font_names <- c("Lora", "Shadows Into Light")
# 
# dir.create("fonts", showWarnings = F)
# 
# purrr::walk(font_names, function(x){
#   
#   ## Download and extract font
#   if (!dir.exists(file.path("fonts", x))) {
#     download.file(
#       url = paste0("https://fonts.google.com/download?family=", x), 
#       destfile = paste0("fonts/", x, ".zip"), 
#       mode = "wb"
#     )
#     unzip(zipfile = paste0("fonts/", x, ".zip"), exdir = file.path("fonts", x))
#     unlink(paste0("fonts/", x, ".zip"))
#   } ## End if download font
#   
#   ## Import fonts to R sysfonts
#   if (!(x %in% names(windowsFonts()))) {
#     extrafont::font_import(paths = "fonts", recursive = T, pattern = str_remove_all(x, " "), prompt = F)
#     extrafont::loadfonts(device = "win")
#   } ## End if add to R sysfonts
#   
# }) ## End walk
#   
# 
# 
# ## Make font easy to use
# library(showtext)
# font_add("Lora", "fonts/Lora/static/Lora-Regular.ttf")
# font_add("LoraIt", "fonts/Lora/static/Lora-Italic.ttf")
# font_add("Shadow", "fonts/Shadows Into Light/ShadowsIntoLight-Regular.ttf")
# showtext_auto()

