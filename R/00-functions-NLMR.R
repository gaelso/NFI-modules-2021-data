## NFI-modules-2021-learnr
## Gael Sola, FAO


## !!! For testing
# tt <- list(.seed = 7, .alt = 2000, .sea = 0.2, .nb_ft = 4, .mg = T, .river = T, .path = "C:/SAGA-GIS", .tmp = F, .quiet = T)
# 
# attach(tt)
## !!!



create_newland <- function(.seed = 11, .alt = 2000, .sea = 0.2, .nb_ft = 4, .mg = T, .river = F, .path = "C:/SAGA-GIS", .tmp = T, .quiet = T){
  
  ## Generate a square country of 90 km length (1000 pix at 90m resolution), with 
  ## predefined location and approximated ratios for different land uses.
  ## 
  ## .seed  : Fix seed for map generation (default 11, tested and looks good).
  ## .alt   : Max altitude of the elevation in m.
  ## .sea   : Ratio between 0 and 0.3. Pixels of the defined proportion will have elevation below 0.
  ## .nb_ft : Number of forest types between 1 and 4.By default 4 forest types are created, 
  ##          then merged to the desired number.
  ## .mg    : Add mangrove? Default TRUE. All lands with elevation in [-10:20] are considered 
  ##          mangrove in one sector of the map. Optimized for seed 11. Randomization to be added.
  ## .river : If saga_cmd available, add river an lakes based on SAGA Togography Wetness Index module.
  ## .path  : folder location of saga_cmd
  ## .tmp   : Boolean. if TRUE store intermediate SAGA outputs in the system temp folder, 
  ##          if FALSE, store in results/.seed subfolder
  ## .quiet : if TRUE remove SAGA functions cat() messages

  message("Initiating data creation...")
  
  
  
  ## Validation Rules #######################################################
  
  ## --- Valid numeric values ----------------------------------------------- 
  # if (!is.numeric(unlist(as.list(environment())))) stop("input variables not all numeric!")
  if (!(is.numeric(.seed)& .seed == round(.seed)))                       stop(".seed must be an integer value.")
  if (!(is.numeric(.alt) & .alt == round(.alt)))                         stop(".alt must be an integer value.")
  if (!(is.numeric(.sea) & .sea >= 0 & .sea <= 0.5))                     stop(".sea must be a numeric value between 0 and 0.5.")
  if (!(is.numeric(.nb_ft) & .nb_ft == round(.nb_ft) & .nb_ft %in% 1:4)) stop(".nb_ft must be an integer value between 1 and 4")
  
  ## --- Valid booleans -----------------------------------------------------
  if (!is.logical(.mg))    stop(".mg must be a boolean TRUE or FALSE")
  if (!is.logical(.river)) stop(".river must be a boolean TRUE or FALSE")
  if (.river) {
    check_saga <- quiet(tryCatch(rsaga.env(path = .path), error=function(e) e))
    if (names(check_saga)[1] == "message") stop("saga_cmd not found in .path, specify the correct path or install SAGA first")
  }
  if (!is.logical(.tmp))   stop(".tmp must be a boolean TRUE or FALSE")
  if (!is.logical(.quiet)) stop(".quiet must be a boolean TRUE or FALSE")
  
  ## Disable warnings
  options(warn = -1)
  
  
  
  ## Setup ##################################################################
  
  ## --- Sizes -------------------------------------------------------------- 
  length <- 90              ## End results length in km
  res    <- 90              ## End results resolution in m
  ll     <- length * 10^3   ## Conversion km to m
  pp     <- round(ll / res) ## End results pixels number
  mp     <- 1000            ## Map pixels number
  ff     <- round(pp/mp)    ## Aggregation/disaggregation factor
  rr     <- res * ff        ## map resolution in m
  
  ## --- Country location setting (Middle of Atlantic Ocean) ----------------
  country_loc <- list(
    crs = "+proj=utm +zone=27 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", 
    xmin = 560363, 
    ymin = 9919901
  )
  
  ## --- Forest types parameters --------------------------------------------
  ft_param <- tibble(
    id   = c(   5,    4,    3,    2),
    lc  = c("EV", "MD", "DD", "PL"),
    w   = c(0.11, 0.23, 0.08, 0.21),
  ) %>%
    bind_rows(list(id = 1, lc = "NF", w = 1 - sum(.$w))) %>%
    arrange(id) %>%
    mutate(
      #hex = c("#d3bdaf", "#6a8763", "#3b633f", "#40624c", "#243a39")
      #hex = c("#edf5e1", "#8ee4af", "#5cdb95", "#379683", "#05386b")
      hex = c("#edf5e1", "#8ee4af", "#5cdb95", "#379683", "#00743f")
    )
  
  ## --- Land cover parameters extension ------------------------------------
  lc_param <- ft_param %>% 
    bind_rows(list(id = c(0, 6), lc = c("WA", "MG"), w = c(0, 0), hex = c("#73c2fb", "#012172"))) %>%
    arrange(id) %>%
    { if (!.mg) dplyr::filter(., id != 6) else .}
  
  #scales::show_col(lc_param$hex)
  
  
  
  ## Make Topo Raster #######################################################
  
  time1 <- Sys.time()
  
  ## --- Make base layers ---------------------------------------------------
  if (!is.null(.seed)) set.seed(.seed)
  topo_mpd <- nlm_mpd(nrow = mp,  ncol = mp, resolution = rr, roughness = 0.4, rescale = T, verbose = F)
  topo_mpd <- crop(topo_mpd, extent(0, ll, 0, ll))
  # topo_mpd
  # plot(topo_mpd)
  
  ## --- Apply sea ratio and max altitude -----------------------------------
  ## --- Translate a percentage of the pixels equal to .sea below 0
  topo_calc <- topo_mpd - quantile(topo_mpd, probs = .sea)
  
  ## --- Apply different max altitude to sea bottom and mountain top
  z_min <- cellStats(topo_calc, "min")
  z_max <- cellStats(topo_calc, "max")
  
  topo_calc[topo_calc <= 0] <- round(topo_calc[topo_calc <= 0] / abs(z_min) * (.alt / 4))
  topo_calc[topo_calc >  0] <- round(topo_calc[topo_calc >  0] / z_max * (.alt))
  
  ## --- Make smoother topo -------------------------------------------------
  #topo <- raster::focal(topo_calc, w = matrix(1, 31, 31), mean, pad = T, padValue = 0)
  topo <- raster::focal(topo_calc, w = matrix(1/31^2, nc=31, nr=31), pad = T, padValue = 0) ## Supposed to be faster
  
  ## Add location
  crs(topo)    <- country_loc$crs
  extent(topo) <- c(country_loc$xmin, country_loc$xmin + ll, country_loc$ymin, country_loc$ymin + ll)
  
  ## --- Sea mask -------------------------------------------------------------
  sea_mask <- topo
  sea_mask[sea_mask > 0] <- NA
  sea_mask[sea_mask <= 0] <- 1
  #plot(sea_mask)
  

  ## Message topo
  time2 <- Sys.time()
  dt <- round(as.numeric(time2-time1, units = "secs"))
  message(paste0("...Topography created", " - ", dt, " Sec."))
  
  
  
  ## Make Land Cover Raster #################################################
  
  time1 <- Sys.time()
  
  ## --- Make base layers at 100 row and cols rasters -----------------------
  if (!is.null(.seed)) set.seed(.seed)
  ft_mpd <- nlm_mpd(nrow = mp,  ncol = mp, resolution = rr, roughness = 0.7, rescale = T, verbose = F)
  ft_mpd <- crop(ft_mpd, extent(0, ll, 0, ll))
  ft_mpd <- aggregate(ft_mpd, fact = 10, fun=mean) 
  
  ft_ne <- nlm_neigh(nrow = mp/10,  ncol = mp/10, resolution = rr*10, 
                     p_neigh = 0.7, p_empty = 0.1,
                     categories = 5, proportions = ft_param$w, neighbourhood = 8)
  
  ft_res <- util_merge(ft_mpd, ft_ne, scalingfactor = 0.1)
  
  # look at the results
  #show_landscape(list("mpd" = ft_mpd, "ne" = ft_ne, "res" = ft_res), n_col = 1)
  
  ## Disaggregate
  ft <- disaggregate(ft_res, fact = 10, method = 'bilinear')
  ft <- util_classify(ft, weighting = ft_param$w, level_names = ft_param$id)

  # par(mfrow = c(2, 1))
  # plot(ft)
  # hist(ft)
  # par(mfrow = c(1, 1))
  
  ## Add location
  crs(ft)    <- country_loc$crs
  extent(ft) <- c(country_loc$xmin, country_loc$xmin + ll, country_loc$ymin, country_loc$ymin + ll)
  
  ## Message Forest type
  time2 <- Sys.time()
  dt <- round(as.numeric(time2-time1, units = "secs"))
  message(paste0("...Forest types created", " - ", dt, " Sec."))
  
  
  
  ## Make river mask ########################################################
  
  if (.river) {
    
    time1 <- Sys.time()
    
    ## Set path for SAGA intermediate files
    if (.tmp) saga_dir <- tempdir() else saga_dir <- paste0("results/", .seed); dir.create(saga_dir, showWarnings = F)
    
    ## --- Remove topo below -5 m -------------------------------------------
    init <- topo
    init[init < -10] <- NA
    
    ## Save topo as SAGA Grid format
    writeRaster(init, file.path(saga_dir, "init.sgrd"), format="SAGA", NAflag=-9999, overwrite = T)
    
    ## --- Get SAGA modules -------------------------------------------------
    #rsaga.get.libraries()
    #rsaga.get.modules(libs = "ta_channels")
    
    ## --- Run SAGA Fill Sinks ----------------------------------------------
    #rsaga.get.usage(lib = "ta_preprocessor", module = 4)
    
    params <- list(ELEV   = file.path(saga_dir, "init.sgrd"),
                   FILLED = file.path(saga_dir, "filled.sdat"))
    
    input <- list(lib = "ta_preprocessor", module = 4, param = params)
    
    if (.quiet)  quiet(do.call(rsaga.geoprocessor, input)) else do.call(rsaga.geoprocessor, input)
  
    filled <- raster::raster(file.path(saga_dir, "filled.sdat"))
    #plot(filled)
    
    # ## !!! NOT USED, gives to much water cover, replaced by strahler order
    # ## --- Run SAGA Wetness Index -------------------------------------------
    # #rsaga.get.usage(lib = "ta_hydrology", module = "SAGA Wetness Index")
    # 
    # params <- list(DEM = file.path(saga_dir, "filled.sgrd"),
    #                TWI = file.path(saga_dir, "twi.sdat"))
    # 
    # input <- list(lib = "ta_hydrology", module = "SAGA Wetness Index", param = params)
    # 
    # if (.quiet)  quiet(do.call(rsaga.geoprocessor, input)) else do.call(rsaga.geoprocessor, input)
    # 
    # ## Load TWI
    # twi <- raster::raster(file.path(saga_dir, "twi.sdat"))
    # #plot(twi)
    # 
    # ## --- Select top 5 areas only ------------------------------------------
    # ## Filter most wet areas from [max(wetness) - 3] and above 
    # twi[twi < round(cellStats(twi, max) - 3)] <- NA
    # #plot(twi, col = RColorBrewer::brewer.pal(n = 9, name = "Blues"))
    # 
    # ## Clump areas to remove smallest areas
    # twi <- raster::clump(twi)
    # 
    # vec <- sort(freq(twi), decreasing = T)[2:6]
    # tib <- as_tibble(freq(twi)) %>% filter(count %in% vec)
    # 
    # ## River mask
    # twi[!(twi %in% tib$value)] <- NA
    # twi[!is.na(twi)] <- 1
    # plot(twi)
    
    ## --- Run SAGA Strahler order ------------------------------------------ 
    #rsaga.get.usage(lib = "ta_channels", module = 6)
    
    params <- list(DEM      = file.path(saga_dir, "filled.sgrd"),
                   STRAHLER = file.path(saga_dir, "strahler.sdat"))
    
    input <- list(lib = "ta_channels", module = 6, param = params)
    
    if (.quiet)  quiet(do.call(rsaga.geoprocessor, input)) else do.call(rsaga.geoprocessor, input)
    
    ## Load stralher
    strahler <- raster::raster(file.path(saga_dir, "strahler.sdat"))
    #plot(strahler)
    
    ## --- Select top 3 rivers only -----------------------------------------
    ## Filter highest orders 
    strahler[strahler < round(cellStats(strahler, max) - 2)] <- NA
    
    ## Clump areas
    strahler <- raster::clump(strahler)
    
    vec <- sort(freq(strahler), decreasing = T)[2:4]
    tib <- as_tibble(freq(strahler)) %>% filter(count %in% vec)
    
    ## River mask
    strahler[!(strahler %in% tib$value)] <- NA
    strahler[!is.na(strahler)] <- 1
    
    ## Add 1 pixel buffer 
    str2 <- buffer(strahler, width = rr)
    
    
    ## Message river
    time2 <- Sys.time()
    dt <- round(as.numeric(time2-time1, units = "secs"))
    message(paste0("...Rivers and lakes created", " - ", dt, " Sec."))
    
    } ## END IF .river
  
  
  
  ## Mangrove mask ##########################################################
  
  if (.mg) {
    
    time1 <- Sys.time()
    
    mg <- topo
    mg[mg <  -10 | mg >  20] <- NA
    mg[mg >= -10 & mg <= 20] <- 1
    mg[,1:600]    <- NA
    mg[700:1000,] <- NA
    
    #show_landscape(mg, discrete = T)
    
    ## Message mangroves
    time2 <- Sys.time()
    dt <- round(as.numeric(time2-time1, units = "secs"))
    message(paste0("...Mangroves added", " - ", dt, " Sec."))
    
  } ## END IF .mg
  
  
  
  ## Combine all land covers ################################################
  
  time1 <- Sys.time()
  
  ## Added land covers based on masks
  lc <- ft
  lc[sea_mask == 1] <- 0 ## LC code for water
  if (.mg)    lc[mg   == 1] <- 6  ## Code for mangroves
  if (.river) lc[str2 == 1] <- 0  ## Code for water
  
  ## Update levels
  lc <- ratify(lc)
  levels(lc) <- lc_param %>% dplyr::select(ID = id, lc) %>% as.data.frame() 
  # lc
  # plot(lc, col = lc_param$hex, legend = FALSE, axes = F)
  # legend("bottomleft",
  #        legend = lc_param$lc,
  #        fill = lc_param$hex)
  
  
  ## Message LC done
  time2 <- Sys.time()
  dt <- round(as.numeric(time2-time1, units = "secs"))
  message(paste0("...Land cover finalized", " - ", dt, " Sec."))
  
  
  
  ## Make 3D map ############################################################
  
  ## Get lc categories as matrix
  # lc_m <- tibble(id = as.vector(flip(lc))) %>%
  #   left_join(lc_param %>% select(id, lc)) %>%
  #   pull(lc) %>%
  #   matrix(., ncol = mp)
  
  ## Water layer
  zz <- cellStats(topo, min)
  water <- list(
    x = c(0,  0, mp, mp,  0,  0, mp, mp),
    y = c(0, mp, mp,  0,  0, mp, mp,  0),
    z = c(0,  0,  0,  0, zz, zz, zz, zz)
  )
  
  out_gr <- plotly::plot_ly(
    z = as.matrix(flip(topo)), 
    lighting = list(ambient = 0.2),
    colors = lc_param$hex
  ) %>%
    plotly::add_surface(
      #colorscale =list(c(0,1), c("#9DC183", "#9DC183")),
      #showscale=F
      surfacecolor = as.matrix(flip(lc))
    ) %>%
    plotly::add_trace(type = "mesh3d", 
              x = water$x,
              y = water$y,
              z = water$z,
              i = c(7, 0, 0, 0, 4, 4, 6, 1, 4, 0, 3, 6),
              j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
              k = c(0, 7, 2, 3, 6, 7, 1, 6, 5, 5, 7, 2),
              facecolor = rep("#73C2FB", 12), 
              opacity = 0.2
    ) %>%
    #plotly::layout(scene = list(aspectmode="data"))
    plotly::layout(scene = list(aspectmode="manual", aspectratio = list(x=1, y=1, z=cellStats(topo, max) / nrow(topo) * 0.1))) ## 0.25

  
  ## Make raster at 30m res
  topo_map <- topo 
  lc_map <- lc
  
  topo <- disaggregate(topo, fact = 3)
  lc  <- disaggregate(lc, fact = 3, method = "")
  levels(lc) <- lc_param %>% dplyr::select(ID = id, lc) %>% as.data.frame() 
  
  ## Put warnings back
  options(warn = 0)
  
  
  
  ## Outputs ################################################################
  return(list(param = lc_param, topo = topo, lc = lc, topo_map = topo_map, lc_map = lc_map, gr = out_gr))
  
} ## End make_topo

