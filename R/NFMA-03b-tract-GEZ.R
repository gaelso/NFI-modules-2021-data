## NFI-modules-2021-learnr
## Gael Sola, FAO

sf_tract <- newtract03tmp3 |>
  st_as_sf(coords = c("tract_long", "tract_lat"), crs = 4326) |>
  st_join(sf_gez2) |>
  mutate(
    gez_name   = case_when(
      iso == "COM" & is.na(gez_name) ~ "Tropical moist forest",
      iso == "BGD" & is.na(gez_name) ~ "Tropical moist forest",
      iso == "GMB" & is.na(gez_name) ~ "Tropical moist forest",
      tract_id == "LBN_941216"       ~ "Subtropical steppe",
      tract_id == "LBN_941024"       ~ "Subtropical dry forest",
      tract_id == "NIC_371"          ~ "Tropical rainforest",
      tract_id == "PHL_841334"       ~ "Tropical rainforest",
      tract_id == "PHL_841364"       ~ "Tropical rainforest",
      tract_id == "PHL_841222"       ~ "Tropical moist forest",
      tract_id == "PHL_841234"       ~ "Tropical rainforest",
      tract_id == "PHL_841216"       ~ "Tropical rainforest",
      tract_id == "PHL_841176"       ~ "Tropical rainforest",
      tract_id == "PHL_841163"       ~ "Tropical rainforest",
      tract_id == "PHL_841169"       ~ "Tropical rainforest",
      tract_id == "PHL_841108"       ~ "Tropical rainforest",
      tract_id == "PHL_841117"       ~ "Tropical rainforest",
      TRUE ~ gez_name
      ),
    gez_abbrev = case_when(
      iso == "COM" & is.na(gez_abbrev) ~ "TAwa",
      iso == "GMB" & is.na(gez_abbrev) ~ "TAwa",
      tract_id == "LBN_941216"         ~ "SBSh",
      tract_id == "LBN_941024"         ~ "SCs",
      tract_id == "NIC_371"            ~ "TAr",
      tract_id == "PHL_841334"         ~ "TAr",
      tract_id == "PHL_841364"         ~ "TAr",
      tract_id == "PHL_841222"         ~ "TAwa",
      tract_id == "PHL_841234"         ~ "TAr",
      tract_id == "PHL_841216"         ~ "TAr",
      tract_id == "PHL_841176"         ~ "TAr",
      tract_id == "PHL_841163"         ~ "TAr",
      tract_id == "PHL_841169"         ~ "TAr",
      tract_id == "PHL_841108"         ~ "TAr",
      tract_id == "PHL_841117"         ~ "TAr",
      TRUE ~ gez_abbrev
      )
  )

## Checks    
table(sf_tract$gez_name, sf_tract$iso, useNA = "always")


## Checks
# walk(unique(sf_tract$iso), function(x){
#   out <- ggplot() +
#     geom_sf(data = sf_tract |> filter(iso == x), aes(color = gez_name, shape = gez_name)) +
#     geom_sf(data = sf_country |> filter(GID_0 == x), fill = NA) +
#     labs(title = x) +
#     theme_void()
#   print(out)
# })


# tmap_mode("view")
# tm_shape(sf_country) + tm_borders() +
# tm_shape(sf_gez2) + tm_polygons(col = "gez_name") +
# tm_shape(sf_tract) + tm_dots(col = "gez_name",
#                              palette = "Dark2",
#                              size = 0.1,
#                              popup.vars = c("iso", "tract_id", "tract_x", "tract_y", "gez_name", "gez_abbrev"),
#                              popup.format = list(tract_x=list(digits=0), tract_y=list(digits=0))
#                              )

## Remove temp objects
rm(newtract03tmp3)
