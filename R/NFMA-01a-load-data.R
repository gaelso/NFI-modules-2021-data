## NFI-modules-2021-learnr
## Gael Sola, FAO



## Load NFMA data ###########################################################

## list files
list_tract <- list.files(path = "data/NFMA/csv", pattern = "F1-Tract", recursive = T, full.names = T)
list_plot  <- list.files(path = "data/NFMA/csv", pattern = "F2-Plot", recursive = T, full.names = T)
list_lus   <- list.files(path = "data/NFMA/csv", pattern = "F5-LUS", recursive = T, full.names = T)
list_tree  <- list.files(path = "data/NFMA/csv", pattern = "F3-Trees", recursive = T, full.names = T)
lu_codes   <- list.files(path = "data/NFMA/csv", pattern = "lu-codes", recursive = T, full.names = T)
sp_codes   <- list.files(path = "data/NFMA/csv", pattern = "species-codes", recursive = T, full.names = T)

## Get country names
country_names <- list_tract |>
  str_remove("/F1-Tract.csv") |>
  str_remove("/F1-Tract_spanish.csv") |>
  str_remove(".*/")

names(list_tract) <- country_names
names(list_plot)  <- country_names
names(list_lus)   <- country_names
names(list_tree)  <- country_names
names(lu_codes)   <- country_names
names(sp_codes)   <- country_names

## Load data and concatenate to data frames
tract01  <- map_dfr(list_tract, read_csv, col_type = cols(.default = col_character()), .id = "country")
plot01   <- map_dfr(list_plot , read_csv, col_type = cols(.default = col_character()), .id = "country")
tree01   <- map_dfr(list_tree , read_csv, col_type = cols(.default = col_character()), .id = "country")
lus01    <- map_dfr(list_lus  , read_csv, col_type = cols(.default = col_character()), .id = "country")
lu_codes <- map_dfr(lu_codes  , read_csv, col_type = cols(.default = col_character()), .id = "country")
sp_codes <- map_dfr(sp_codes  , read_csv, col_type = cols(.default = col_character()), .id = "country")


## country 3 digits ISO code
country_iso <- tibble(
  country = country_names,
  iso     = c("AGO", "BGD", "CMR", "COM", "COG", "CRI", "GMB", "GTM", "HND", "KEN", "KGZ", "LBN", "NIC", "PHL", "ZMB"),
  region  = c("AF" , "AP" , "AF" , "AF" , "AF" , "LC" , "AF" , "LC" , "LC" , "AF" , "EU" , "NE" , "LC" , "AP" , "AF")
)


## Manual correction irregular land use codes
## !! TO BE CONTINUED !!
tt <- lu_codes |> filter(is.na(lu))
table(tt$country)

lu_codes <- lu_codes |>
  mutate(
    lu = case_when(
      country == "Zambia" & id == "110010" ~ "Evergreen Forest",
      country == "Zambia" & id == "110020" ~ "Deciduous Forest",
      country == "Zambia" & id == "110030" ~ "Semi-evergreen Forest",
      TRUE ~ lu
    )
  )


