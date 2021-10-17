
## Main script to call functions and scripts for:
## - Making the mock country forest type raster layers
## - Preparing the NFI data to suit the exercises




## Source scripts ###########################################################

##
## SETUP ####################################################################
##

source('R/00-libs.R')

source('R/00-functions-misc.R')

source('R/00-functions-NFMA.R')

source("R/00-functions-NLMR.R")

source("R/00-functions-3D.R")

#source("R/00-functions-NLMR-topo2.R")

#source("R/00-functions-NLMR-lu.R")

#source('R/00-graphic_style.R')

source("R/00-paths.R")


##
## DATA preparation #########################################################
##

## --- Create mock plot and tree data based on NFMA raw data ----------------

#source('R/NFMA-download.R') ## Aborted, requires admin authorization to connect to sharepoint, downloaded from sharepoint manually

#source('R/NFMA-convert-mdb.R') ## Require Linux package mdbtools - Done with WSL on Windows

#source('R/NFMA-convert-accdb.R') ## Aborted require moving DB to specific folder

source('R/NFMA-01a-load-data.R')

source('R/NFMA-01b-load-auxiliary.R')

source('R/NFMA-02a-simplify-tract.R')

source('R/NFMA-02b-simplify-plot.R')

source('R/NFMA-02c-simplify-lus.R')

source('R/NFMA-02d-simplify-tree.R')

source('R/NFMA-02e-simplify-newplot.R')

source('R/NFMA-03a-tract-coords.R')

source('R/NFMA-03b-tract-GEZ.R')

source('R/NFMA-03c-tract-chaveE.R')

source('R/NFMA-04-newplot-lus.R')

source('R/NFMA-05-tree-position.R')

source('R/NFMA-06-tree-species.R')

source('R/NFMA-07-tree-wd.R')

source('R/NFMA-08-tree-dbh-h.R')

source('R/NFMA-09-tree-agb.R')

source('R/NFMA-10-plot-agb.R')

source('R/NFMA-11-forest-agb.R')

source('R/NFMA-12-prepa-mock.R')

##
## Create new land basded on NLMR ###########################################
##

## NLMR
source('R/NLMR-create-countries.R')
