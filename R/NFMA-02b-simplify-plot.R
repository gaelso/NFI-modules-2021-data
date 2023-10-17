## NFI-modules-2021-learnr
## Gael Sola, FAO

## Extract plot coordinates #################################################

## Check plot column names
# tt <- names(plot_init)
# plot_init |> filter(!is.na(X39a.PlotStartingPointLatitude)) |> pull(country) |> unique()  ## BGD, COM, GMB, KRG, LBN, PHL, ZMB
# plot_init |> filter(!is.na(X39d.PlotStartingPointX)) |> pull(country) |> unique()  ## AGO, COM, GMB, KRG, LBN, PHL, ZMB
# plot_init |> filter(!is.na(Plot.LatitudeField)) |> pull(country) |> unique()  ## CRI
# plot_init |> filter(!is.na(latitudinicio)) |> pull(country) |> unique()  ## HND
# plot_init |> filter(!is.na(UTMX.MP)) |> pull(country) |> unique()  ## NIC
# 
# plot_init |> filter(!is.na(ID.PLOT)) |> pull(country) |> unique()  ## missing CRI, NIC, HND

plot02 <- plot01 |>
  left_join(country_iso, by = "country") |>
  mutate(
    tract_id = case_when(
      !is.na(ID.TRACT)       ~ paste0(iso, "_", ID.TRACT),
      !is.na(Tract.ID)       ~ paste0(iso, "_", Tract.ID),
      !is.na(unidadmuestreo) ~ paste0(iso, "_", unidadmuestreo),
      !is.na(ID.UM)          ~ paste0(iso, "_", ID.UM),
      TRUE ~ NA_character_
    ),
    plot_id = case_when(
      !is.na(ID.PLOT)    ~ paste0(iso, "_", ID.PLOT),
      !is.na(Plot.ID)    ~ paste0(iso, "_", as.numeric(Tract.ID) * 100 + as.numeric(Plot.ID)),
      !is.na(parcela)    ~ paste0(iso, "_", as.numeric(unidadmuestreo) * 100 + as.numeric(parcela)),
      !is.na(ID.PARCELA) ~ paste0(iso, "_", as.numeric(ID.UM) * 100 + as.numeric(ID.PARCELA)),
      TRUE ~ NA_character_
    ),
    plot_no = case_when(
      !is.na(X3.PlotNo)  ~ as.numeric(X3.PlotNo),
      !is.na(Plot.ID)    ~ as.numeric(Plot.ID),
      !is.na(parcela)    ~ as.numeric(parcela),
      !is.na(ID.PARCELA) ~ as.numeric(ID.PARCELA),
      TRUE ~ NA_real_
    ),
    plot_lat = case_when(
      !is.na(X39a.PlotStartingPointLatitude) ~ as.numeric(X39a.PlotStartingPointLatitude),
      !is.na(Plot.LatitudeField)             ~ as.numeric(Plot.LatitudeField),
      TRUE ~ NA_real_
    ),
    plot_long = case_when(
      !is.na(X39b.PlotStartingPointLongitude) ~ as.numeric(X39b.PlotStartingPointLongitude),
      !is.na(Plot.LongitudeField)             ~ as.numeric(Plot.LongitudeField),
      TRUE ~ NA_real_
    ),
    plot_x_tmp = case_when(
      !is.na(X39d.PlotStartingPointX)       ~ as.numeric(X39d.PlotStartingPointX),
      !is.na(gt39c.125mUTMX) & plot_no == 1 ~ as.numeric(gt39c.125mUTMX)      , ## GTM
      !is.na(gt39c.125mUTMX) & plot_no == 2 ~ as.numeric(gt39c.125mUTMX) - 125, ## GTM
      !is.na(gt39c.125mUTMX) & plot_no == 3 ~ as.numeric(gt39c.125mUTMX)      , ## GTM
      !is.na(gt39c.125mUTMX) & plot_no == 4 ~ as.numeric(gt39c.125mUTMX) + 125, ## GTM
      !is.na(longitudinico)                 ~ as.numeric(longitudinico),        ## HND
      !is.na(UTMX.MP) & plot_no == 1        ~ as.numeric(UTMX.MP)      , ## NIC
      !is.na(UTMX.MP) & plot_no == 2        ~ as.numeric(UTMX.MP) - 125, ## NIC
      !is.na(UTMX.MP) & plot_no == 3        ~ as.numeric(UTMX.MP)      , ## NIC
      !is.na(UTMX.MP) & plot_no == 4        ~ as.numeric(UTMX.MP) + 125, ## NIC
      TRUE ~ NA_real_
    ),
    plot_y_tmp = case_when(
      !is.na(X39c.PlotStartingPointY) ~ as.numeric(X39c.PlotStartingPointY),
      !is.na(gt39d.125mUTMY)          ~ as.numeric(gt39d.125mUTMY), ## GTM
      !is.na(latitudinicio)           ~ as.numeric(latitudinicio), ## HND
      !is.na(UTMY.MP) & plot_no == 1        ~ as.numeric(UTMY.MP) - 125, ## NIC
      !is.na(UTMY.MP) & plot_no == 2        ~ as.numeric(UTMY.MP)      , ## NIC
      !is.na(UTMY.MP) & plot_no == 3        ~ as.numeric(UTMY.MP) + 125, ## NIC
      !is.na(UTMY.MP) & plot_no == 4        ~ as.numeric(UTMY.MP)      , ## NIC
      TRUE ~ NA_real_
    ),
    plot_x = case_when(
      plot_x_tmp == 0 ~ NA_real_,
      iso == "GTM" & plot_no == 1 ~ plot_x_tmp,
      iso == "GTM" & plot_no == 2 ~ plot_x_tmp - 125,
      iso == "GTM" & plot_no == 3 ~ plot_x_tmp,
      iso == "GTM" & plot_no == 4 ~ plot_x_tmp + 125,
      iso == "NIC" & plot_no == 1 ~ plot_x_tmp,
      iso == "NIC" & plot_no == 2 ~ plot_x_tmp - 125,
      iso == "NIC" & plot_no == 3 ~ plot_x_tmp,
      iso == "NIC" & plot_no == 4 ~ plot_x_tmp + 125,
      TRUE ~ plot_x_tmp,
    ),
    plot_y = case_when(
      plot_y_tmp == 0 ~ NA_real_,
      iso == "GTM" & plot_no == 1 ~ plot_y_tmp - 125,
      iso == "GTM" & plot_no == 2 ~ plot_y_tmp,
      iso == "GTM" & plot_no == 3 ~ plot_y_tmp + 125,
      iso == "GTM" & plot_no == 4 ~ plot_y_tmp,
      iso == "NIC" & plot_no == 1 ~ plot_y_tmp - 125,
      iso == "NIC" & plot_no == 2 ~ plot_y_tmp,
      iso == "NIC" & plot_no == 3 ~ plot_y_tmp + 125,
      iso == "NIC" & plot_no == 4 ~ plot_y_tmp,
      TRUE ~ plot_y_tmp
    )
  ) |>
  left_join(tract02, by = c("iso", "tract_id")) |>
  mutate(
    epsg = case_when(
      !is.na(epsg)                       ~ epsg,
      !is.na(plot_long) & plot_lat >= 0  ~ 32600 + round((plot_long + 180)/6, 0),
      !is.na(plot_long) & plot_lat <  0  ~ 32700 + round((plot_long + 180)/6, 0),
      TRUE ~ NA_real_
    ),
    epsg_default = case_when(
      iso == "AGO" ~ 32733,
      iso == "BGD" ~ 32645,
      iso == "CMR" ~ 32632,
      iso == "COG" ~ 32733,
      iso == "COM" ~ 32738,
      iso == "CRI" ~ 32616,
      iso == "GMB" ~ 32628,
      iso == "GTM" ~ 32615,
      iso == "HND" ~ 32616,
      iso == "KEN" ~ 32637,
      iso == "KGZ" ~ 32643,
      iso == "LBN" ~ 32636,
      iso == "NIC" ~ 32616,
      iso == "PHL" ~ 32651,
      iso == "ZMB" ~ 32735,
      TRUE ~ NA_real_
    ),
    epsg_corr = case_when(
      #      iso == "COM" ~ epsg_default, ## Error in epsg calculation, utm zone 37 instead of 38
      !is.na(epsg) ~ epsg,
      TRUE         ~ epsg_default
    )
  ) |>
  filter(
    !(is.na(plot_x) & is.na(plot_long)),
    !(is.na(plot_y) & is.na(plot_lat ))
  ) |>
  dplyr::select(iso, tract_id, plot_id, plot_no, plot_lat, plot_long, plot_x, plot_y, epsg, epsg_corr)


## Checks
table(plot02$iso, plot02$plot_no, useNA = "always")

# table(plot$plot_no, useNA = "always")
# check <- plot |> filter(is.na(tract_id))
# table(check$iso)
# check <- plot |> filter(is.na(plot_id))
# table(check$iso)

