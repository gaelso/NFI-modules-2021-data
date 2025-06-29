
library(sf)
library(terra)
library(geosphere)
library(tidyterra)
library(tidyverse)

theme_set(theme_bw())

##
## LOAD DATA ####
##

## + Load NFMA tree data ####

path_nfma       <- list.files("data/Gambia", pattern = "GMB-.*\\.csv", full.names = T)
path_nfma_names <- list.files("data/Gambia", pattern = "GMB-.*\\.csv") |> str_remove("GMB-") |> 
  str_remove("\\.csv")

nfma <- map(path_nfma, read_csv, show_col_types = F)
names(nfma) <- path_nfma_names

walk(path_nfma_names, function(x){
  assign(paste0(x, "_init"), nfma[[x]], envir = globalenv())
})

## + Get Chave 2014 E ####

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

## + Get WD at species level ####

if (!"wdData.csv" %in% list.files("data/Gambia")) {
  download.file(
    url = "https://raw.githubusercontent.com/umr-amap/BIOMASS/refs/heads/master/data-raw/wdData.csv",
    destfile = file.path("data/Gambia", "wdData.csv")
  )
}

wd_init <- read_csv("data/Gambia/wdData.csv", show_col_types = F) |>
  select(wd_no = Number, wd_family_name = Family, wd_species_name = Binomial, wd_gcm3 = `Wood density (g/cm^3), oven dry mass/fresh volume`, wd_region = Region)

table(wd_init$Region)


## + Get CEO data for strata weights ####

ceo_extra <- read_csv("results/Gambia/ceo_extra.csv")

ceo_tract_corr <- read_csv("results/Gambia/t4_corr.csv")

ceo_corr <- ceo_extra |>
  left_join(ceo_tract_corr) |>
  mutate(lu_class = if_else(!is.na(lu_corr), lu_corr, ceo_lu_class))


## Get country boundary
sf_country <- st_read("data/Gambia/Admin boundries/LG_Areas.shp") |> 
  st_transform(4326)

plot(sf_country)

sf_country2 <- sf_country |>
  summarise()

plot(sf_country2)

gb_area <- as.numeric(st_area(sf_country2))/10000


## 
## PRELIMINARY CALCULATIONS ####
##

## + Make species WD averages for Africa ####

wd_sp <- wd_init |> 
  filter(wd_region == "Africa (tropical)") |>
  group_by(wd_species_name) |>
  summarise(
    count_avg_species = n(),
    wd_avg_species = mean(wd_gcm3), 
    wd_std_species = sd(wd_gcm3),
    .groups = "drop"
    )

wd_gn <- wd_init |> 
  filter(wd_region == "Africa (tropical)") |>
  mutate(wd_genus_name = word(wd_species_name)) |>
  group_by(wd_genus_name) |>
  summarise(
    count_avg_genus = n(),
    wd_avg_genus = mean(wd_gcm3), 
    wd_std_genus = sd(wd_gcm3),
    .groups = "drop"
  )

wd_fm <- wd_init |> 
  filter(wd_region == "Africa (tropical)") |>
  group_by(wd_family_name) |>
  summarise(
    count_avg_family = n(),
    wd_avg_family = mean(wd_gcm3), 
    wd_std_family = sd(wd_gcm3),
    .groups = "drop"
  )

wd_unk <- wd_init |> 
  filter(wd_region == "Africa (tropical)") |>
  summarise(
    count_avg_unknown = n(),
    wd_avg_unknown = mean(wd_gcm3), 
    wd_std_unknown = sd(wd_gcm3),
    .groups = "drop"
  )


## + Get land use section areas ####
lus <- lus_init |>
  mutate(area_ha = round(plot_length * plot_width / 10000, 3)) |>
  select(tract_no, plot_no, lus_no, lus, lus_class, area_ha)

## + Get E at plot level ####
sf_plot_gps <- plot_gps_init |>
  mutate(
    tract_no = as.numeric(str_sub(tract_id, -3)),
    plot_no  = as.numeric(str_sub(plot_id, -1)),
    x = plot_gps_x_middlepoint, 
    y = plot_gps_y_middlepoint
    ) |>
  select(tract_no, plot_no, x, y) |>
  filter(!is.na(x), !is.na(y)) |>
  st_as_sf(coords = c("x", "y"), crs = 32628) |>
  st_transform(crs = 4326)

tmp_E <- terra::extract(rs_E, vect(sf_plot_gps))

# ggplot() +
#   geom_spatraster(data = rs_E) +
#   geom_sf(data = sf_plot_gps) +
#   coord_sf(
#     xlim = st_bbox(sf_plot_gps)[c("xmin", "xmax")],
#     ylim = st_bbox(sf_plot_gps)[c("ymin", "ymax")]
#     )

plot_E <- sf_plot_gps |>
  bind_cols(E = tmp_E[,2]) |>
  as_tibble() |>
  select(-geometry)


## + Get LUS and E at tree level ####

tree <- tree_init |>
  mutate(
    tract_no = as.numeric(str_sub(plot_id, 8, 10)),
    plot_no = as.numeric(str_sub(plot_id, start = -1)),
    tree_genus_name = word(tree_species_name),
    tree_species_epithet = word(tree_species_name, 2),
    tree_species_short = paste(tree_genus_name, tree_species_epithet)
    ) |>
  select(country, iso, tract_no, plot_no, lus_no, everything()) |>
  left_join(lus, by = join_by(tract_no, plot_no, lus_no)) |>
  left_join(plot_E, by = join_by(tract_no, plot_no)) |>
  left_join(wd_sp, by = join_by(tree_species_short == wd_species_name)) |>
  left_join(wd_gn, by = join_by(tree_genus_name == wd_genus_name)) |>
  mutate(
    tree_wd = case_when(
      !is.na(wd_avg_species) ~ wd_avg_species,
      !is.na(wd_avg_genus) ~ wd_avg_genus,
      TRUE ~ wd_unk$wd_avg_unknown
    ),
    tree_wd_level = case_when(
      !is.na(wd_avg_species) ~ "species",
      !is.na(wd_avg_genus) ~ "genus",
      TRUE ~ "regional"
    ),
    tree_height_chave = exp(0.243^2/2)* exp(0.893 - E + 0.760*log(tree_dbh) - 0.0340*(log(tree_dbh))^2),
    tree_h_ciup = tree_height_chave * exp(1.96 * 0.243),
    tree_h_cilo = tree_height_chave * exp(-1.96 * 0.243),
    tree_agb_chave_noh = exp(-1.803 - 0.976*E + 0.976*log(tree_wd) + 2.673*log(tree_dbh) - 0.0299*(log(tree_dbh))^2),
    tree_agb_chave = 0.0673 * (tree_wd * tree_dbh^2 * tree_height_top)^0.976
    )


## Checks

tree |>
  filter(plot_no == 1) |>
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = tree_height_top))


tree |>
  filter(tree_dbh < 100) |>
  filter(plot_no == 1) |>
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = tree_height_top, colour = as.character(tree_health)), alpha = 0.6) +
  facet_wrap(~tree_health)

tree |>
  filter(tree_dbh < 100) |>
  filter(plot_no == 1) |>
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = tree_height_top, colour = as.character(tree_health))) +
  geom_line(aes(y = tree_height_chave, colour = plot_id)) +
  geom_line(aes(y = tree_h_ciup, colour = plot_id)) +
  geom_line(aes(y = tree_h_cilo, colour = plot_id)) +
  theme(legend.position = "none")

table(tree$tree_health, useNA = "ifany")

tree |>
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = tree_agb_chave))


## 
## MAIN CALCULATIONS ####
##

## + Cstock per land cover class ####

plot_lus_agb <- tree |>
  #filter(plot_no == 1) |>
  #group_by(tract_no, lus_class, lus,  area_ha) |>
  group_by(tract_no, plot_no, lus_class, lus,  area_ha) |>
  summarise(
    count = n(),
    lus_agb = sum(tree_agb_chave) / 1000, 
    .groups = "drop" 
  ) 
  
tract_agb <- plot_lus_agb |>
  group_by(tract_no, lus_class, lus) |>
  summarise(
    tree_count = sum(count),
    area_ha = sum(area_ha),
    plot_agb = sum(lus_agb), 
    .groups = "drop"
  ) |>
  mutate(
    plot_agb_ha = round(plot_agb / area_ha,  3)
  ) |>
  filter(!is.na(plot_agb_ha))

tract_agb |>
  ggplot(aes(x = lus_class)) +
  geom_boxplot(aes(y = plot_agb_ha)) +
  geom_jitter(aes(y = plot_agb_ha), alpha = 0.6, col = "darkred") +
  coord_flip()

  
class_agb <- tract_agb |>
  group_by(lus_class) |>
  summarise(
    tract_count = n(),
    agb_ha = mean(plot_agb_ha),
    agb_sd = sd(plot_agb_ha)
  )

class_agb

overall_agb <- tract_agb |>
  filter(lus_class != "nonforest") |>
  summarise(
    tract_count = n(),
    agb_ha = mean(plot_agb_ha),
    agb_sd = sd(plot_agb_ha)
  )


## + Overall sample size ####
E <- 10
t <- 1.96 

n <- ((overall_agb$agb_sd / overall_agb$agb_ha * 100) * t/E)^2
n

## + Stratified sampling with Neyman allocation ####


n_ceoplot <- nrow(ceo_plot)

strata_weight <- ceo_corr |>
  filter(lu_class != "nonforest") |>
  summarise(ceo_count = n(), .by = "lu_class") |>
  mutate(weight = ceo_count / n_ceoplot) |>
  left_join(class_agb, by = join_by(lu_class == lus_class)) |>
  mutate(
    WhSh = weight * agb_sd,
    mean_prop = weight * agb_ha
    )

sum_WhSh <- sum(strata_weight$WhSh)
mean_st  <- sum(strata_weight$mean_prop)

n_st <- t^2 * sum_WhSh^2 / (mean_st * E / 100)^2
n_st

## + fixed sample size allocation ####

n_cost <- 120

strata_ss <- strata_weight |>
  mutate(nh = ceiling(n_cost * WhSh / sum_WhSh))

E_st <-  t * sum_WhSh / (mean_st / 100 * sqrt(n_cost))


