## NFI-modules-2021-learnr
## Gael Sola, FAO


## Create mock land and visualizations
#remotes::install_github("ropensci/NLMR")
#remotes::install_version("NLMR", "0.4")
#remotes::install_github("tylermorganwall/rayshader")
library(NLMR)
library(landscapetools)
library(raster)
#library(igraph)
#library(RSAGA)
library(sf)
library(stars)
library(rayshader)
library(rgl)
library(plotly)
library(scales)
library(smoothr)

## Tidy
library(ggpubr)
library(ggrepel)
library(tmap)
library(stringi)
library(tidyverse)

options(dplyr.summarise.inform = FALSE)


