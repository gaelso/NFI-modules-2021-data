## NFI-modules-2021-learnr
## Gael Sola, FAO

## Add Environmental stress #################################################


sf_tract$envir_stress <- raster::extract(envir_stress, sf_tract)


summary(sf_tract$envir_stress)
tt <- sf_tract %>% filter(is.na(envir_stress))
table(tt$iso)

if (nrow(tt) > 0) {
  
  tt$envir_stress <- raster::extract(envir_stress, tt, buffer = 20000, fun = mean)
  
  tt %>% filter(is.na(envir_stress))
  
  newtract03tmp <- sf_tract %>%
    filter(!is.na(envir_stress)) %>%
    bind_rows(tt)
  
} else {
  
  newtract03tmp <- sf_tract
  
}

newtract03tmp %>% filter(is.na(envir_stress))

## Simplify newtract
newtract03 <- newtract03tmp %>%
  mutate(
    tract_long = st_coordinates(.)[,1],
    tract_lat  = st_coordinates(.)[,2]
  ) %>%
  as_tibble() %>%
  select(
    iso, tract_id, epsg_corr, tract_x, tract_y, tract_long, tract_lat, 
    nb_plot, plot_no_min, gez_code, gez_name, gez_abbrev, envir_stress
    )

## Checks
summary(newtract03$envir_stress)
