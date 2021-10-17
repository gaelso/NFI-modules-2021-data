## NFI-modules-2021-learnr
## Gael Sola, FAO


## Extract EPSG from Tract ##########################################################

## Tract coordinates is recalculated from plot coordinates.
## Tract GEZ will be re-assigned based on GEZ 2010 Shapefile.

## Check column names
tt <- names(tract01)

tract01 %>% filter(!is.na(X13b.GlobalZone)) %>% select(country, X13b.GlobalZone) %>% unique()

## Get only global zone from tract. coordinates recalculated from plot later
tract02 <- tract01 %>%
  left_join(country_iso, by = "country") %>%
  mutate(
    tract_id = case_when(
      !is.na(ID.TRACT)       ~ paste0(iso, "_", ID.TRACT),
      !is.na(Tract.ID)       ~ paste0(iso, "_", Tract.ID),
      !is.na(unidadmuestreo) ~ paste0(iso, "_", unidadmuestreo),
      !is.na(ID.UM)          ~ paste0(iso, "_", ID.UM),
      TRUE ~ NA_character_
    ),
    epsg = case_when(
      !is.na(X13b.GlobalZone) & as.numeric(X13b.GlobalZone) <  0 ~ 32700 + abs(as.numeric(X13b.GlobalZone)),
      !is.na(X13b.GlobalZone) & as.numeric(X13b.GlobalZone) >= 0 ~ 32600 + as.numeric(X13b.GlobalZone), 
      TRUE ~ NA_real_
    )
  ) %>% 
  select(iso, tract_id, epsg)

## Checks
table(tract02$epsg, tract02$iso, useNA = "always")
