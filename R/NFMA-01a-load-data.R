## NFI-modules-2021-learnr
## Gael Sola, FAO



## Load NFMA data ###########################################################

## list files
list_tract <- list.files(path = "data/NFMA/csv", pattern = "F1-Tract", recursive = T, full.names = T)
list_tract

list_plot <- list.files(path = "data/NFMA/csv", pattern = "F2-Plot", recursive = T, full.names = T)
list_plot

list_lus <- list.files(path = "data/NFMA/csv", pattern = "F5-LUS", recursive = T, full.names = T)
list_lus

list_tree <- list.files(path = "data/NFMA/csv", pattern = "F3-Trees", recursive = T, full.names = T)
list_tree

lu_codes <- list.files(path = "data/NFMA/csv", pattern = "lu-codes", recursive = T, full.names = T)
lu_codes

sp_codes <- list.files(path = "data/NFMA/csv", pattern = "species-codes", recursive = T, full.names = T)
sp_codes

## Get country names
country <- list_tract %>%
  str_remove("/F1-Tract.csv") %>%
  str_remove("/F1-Tract_spanish.csv") %>%
  str_remove(".*/")

names(list_tract) <- country
names(list_plot)  <- country
names(list_lus)   <- country
names(list_tree)  <- country
names(lu_codes)   <- country
names(sp_codes)   <- country

## Load data and concatenate to data frames
tract01 <- map_dfr(list_tract, read_csv, col_type = cols(.default = col_character()), .id = "country")
plot01  <- map_dfr(list_plot , read_csv, col_type = cols(.default = col_character()), .id = "country")
tree01  <- map_dfr(list_tree , read_csv, col_type = cols(.default = col_character()), .id = "country")
lus01   <- map_dfr(list_lus  , read_csv, col_type = cols(.default = col_character()), .id = "country")
lu_codes   <- map_dfr(lu_codes, read_csv, col_type = cols(.default = col_character()), .id = "country")
sp_codes   <- map_dfr(sp_codes, read_csv, col_type = cols(.default = col_character()), .id = "country")



## country 2 digit ISO code
country_iso <- tibble(
  country = country,
  iso = c("AGO", "BGD", "CMR", "COM", "COG", "CRI", "GMB", "GTM", "HND", "KEN", "KGZ", "LBN", "NIC", "PHL", "ZMB")
)


## Manual correction irregular land use codes
tt <- lu_codes %>% filter(is.na(lu))
table(tt$country)

lu_codes <- lu_codes %>%
  mutate(
    lu = case_when(
      country == "Zambia" & id == "110010" ~ "Evergreen Forest",
      country == "Zambia" & id == "110020" ~ "Deciduous Forest",
      country == "Zambia" & id == "110030" ~ "Semi-evergreen Forest",
      TRUE ~ lu
    )
  )


