###
### Test adding tree height with Chave et al. 2014 model with environmental stress E variable from raster
### 


## Setup

library(tidyverse)
library(ncdf4)
library(sf)
library(raster)
library(tmap)
library(ggpubr)


theme_set(theme_bw())

## Load the data

## !!! For this session because the data from NFI is already good, 
##     we will introduce randomly some error !!!

tree_init <- read_csv("data/tree_all.csv") 

subplot_init <- read_csv("data/subplot_all.csv")

plot_init <- read_csv("data/plot_coord.csv")

## Make subplot spatial object:
sf_subplot <- subplot_init %>% 
  filter(!is.na(GPS_latitude)) %>%
  st_as_sf(coords = c("GPS_longitude", "GPS_latitude"), crs = 4326)

## Load admin boundaries from:
## https://gadm.org/
sf_lao <- st_read(dsn = "data/GIS/LAO_adm/LAO_adm0.shp", quiet = TRUE) 

## Load E raster file Download E.nc from: 
## http://chave.ups-tlse.fr/pantropical_allometry/E.nc.zip
envir_stress <- raster("data/GIS/E.nc")

str(envir_stress)

## Crop E to Lao boundaries
es_lao <- crop(envir_stress, sf_lao)

## CHECK
tmap_mode("plot")

es_plot <- tm_shape(es_lao) +
  tm_raster(palette = terrain.colors(5), title = "Environemental Stress", 
            legend.show = TRUE, style = "cont") + 
  tm_shape(sf_lao) +
  tm_borders(lwd = 2) + 
  tm_shape(sf_subplot) + 
  tm_symbols(shape = 1, scale = 0.3, col = "black") + 
  tm_layout(legend.frame = TRUE, legend.position = c("right", "top"))
es_plot


## Extract E value to subplot level
sf_subplot$es <- raster::extract(es_lao, sf_subplot)


## Estimate tree Height
set.seed(15)
tree <- tree_init %>%
  left_join(as_tibble(sf_subplot)) %>%
  filter(
    class == "live tree",
    lc_class %in% c("EF", "MDF", "DD", "CF", "MCB")) %>%
  dplyr::select(file, tree_id, dbh, subplot_no, plot_id, subplot_id, lc_class, province_name, es) %>%
  mutate(
    es         = if_else(is.na(es), 0, es),
    h_model    = exp(0.893 + 0.760 * log(dbh) - 0.0340 * (log(dbh))^2),
    h_model_es = exp(0.893 - es + 0.760 * log(dbh) - 0.0340 * (log(dbh))^2),
    h_res      = exp(rnorm(n = dim(.)[1], mean = 0, sd = 0.243)),
    h_error    = sample(x = c(rep(1, 9), 2), dim(.)[1], replace = TRUE),
    h_est      = h_model_es * h_res,
    h          = h_est * h_error
  )
tree

g1 <- ggplot(tree, aes(x = dbh)) +
  geom_line(aes(y = h_model)) +
  labs(subtitle = "Chave2014 with E = 0") + 
  ylim(0,60)

g2 <- ggplot(tree, aes(x = dbh)) +
  geom_line(aes(y = h_model_es, color = as.factor(round(es, 2)))) +
  theme(legend.position = 'none') +
  labs(subtitle = "Chave2014 with E from raster") +
  ylim(0,60)

g3 <- ggplot(tree, aes(x = dbh)) +
  geom_point(aes(y = h_est, color = as.factor(round(es, 2))), size = 1) +
  theme(legend.position = 'none') +
  labs(subtitle = "Chave2014 with E and RSE") +
  ylim(0,60)

ggarrange(g1, g2, g3, nrow = 1, align = "hv")



tree_sub <- tree %>% filter(plot_id %in% c(1, 441))


## Test remodelling trees
library(nlme)
library(lmfor)

tree_m <- tree %>% mutate(g = "a")

start <- startHDgomperz(d = tree$dbh, h = tree$h_est)

m1 <- nlme(
  model = h_est ~ 1.3 + a * exp(- b * exp( - c * dbh)), 
  data = tree_m, 
  fixed = a + b + c ~ 1,
  groups = ~ g, 
  start = start, 
  weights = varPower(form = ~ dbh)
  )

summary(m1)
summary(m1)$sigma
AIC(m1)

m2 <- nlme(
  model = h_est ~ 1.3 + a * exp(- b * exp( - c * dbh)), 
  data = tree_m, 
  fixed = a + b + c ~ 1,
  groups = ~ lc_class, 
  start = start, 
  weights = varPower(form = ~ dbh)
)

summary(m2)
AIC(m2)
fixef(m2)
ranef(m2)

str(summary(m2))
summary(m2)$tTable[1,3]
summary(m2)$varStruct


tree_m <- tree_m %>%
  mutate(
    fit.m2  = fitted.values(m2),
    res.m2  = residuals(m2),
    resw.m2 = res.m2 / dbh^summary(m2)$modelStruct$varStruct[1]
  )

ggplot(tree_m) +
  geom_line(aes(x = dbh, y = fit.m2, color = lc_class))

ggplot(tree_m) +
  geom_point(aes(x = fit.m2, y = res.m2, color = lc_class))

ggplot(tree_m) +
  geom_point(aes(x = fit.m2, y = resw.m2, color = lc_class))

SSE <- sum(tree_m$resw.m2^2)

RSE <- sqrt(SSE / summary(m2)$tTable[1,3])
RSE

summary(m2)$sigma

start <- c(0.893, 0.760, 0.034)

m3 <- nlme(
  model = h_est ~ exp(a - es + b * log(dbh) - c * (log(dbh))^2),
  data = tree_m, 
  fixed = a + b + c ~ 1,
  groups = ~ g, 
  start = start, 
  weights = varPower(form = ~ dbh)
)

summary(m3) 
AIC(m3)
summary(m3)$sigma 



m4 <- nlme(
  model = h_est ~ exp(a - es + b * log(dbh) - c * (log(dbh))^2),
  data = tree_m, 
  fixed = a + b + c ~ 1,
  groups = ~ lc_class, 
  start = start, 
  weights = varPower(form = ~ dbh)
)

summary(m4)
AIC(m4)
summary(m4)$sigma


