## NFI-modules-2021-learnr
## Gael Sola, FAO



## Filter countries #########################################################

sf_country <- sf_country |> filter(GID_0 %in% unique(newplot02$iso))


##
## Recreate tract coords from plot ##########################################
##

## --- Identify coord system used -------------------------------------------
check_coord <- newplot02 |>
  dplyr::select(iso, plot_long, plot_lat,  plot_x, plot_y, epsg, epsg_corr) |>
  group_by(iso) |>
  summarise(across(where(is.numeric), mean, na.rm = T)) 
print(check_coord)


## --- Create tract table ---------------------------------------------------
check_plot <- newplot02 |>
  group_by(iso, tract_id) |>
  summarise(
    plot_no_min = min(plot_no),
    nb_plot     = n()
  ) |>
  ungroup()

newtract03tmp <- newplot02 |>
  left_join(check_plot, by = c("iso", "tract_id")) |>
  filter(plot_no == plot_no_min) |>
  mutate(
    x_corr = case_when(
      iso == "HND" &  plot_x > 10^6 ~ plot_y,
      TRUE ~ plot_x
    ),
    y_corr = case_when(
      iso == "HND" &  plot_y < 10^6 ~ plot_x,
      iso == "HND" &  plot_y > 10^7 ~ plot_y - 17100000 + 1700000,
      TRUE ~ plot_y
    ),
    epsg_corr = case_when(
      tract_id %in% c("HND_207", "HND_210", "HND_242")       ~ 32617,
      iso == "GTM" & epsg_corr == 32616 & plot_x >= 4 * 10^5 ~ 32615,
      iso == "GTM" & epsg_corr == 32615 & plot_x <= 6 * 10^5 ~ 32616,
      iso == "NIC" & epsg_corr == 32616 & plot_x <= 4 * 10^5 ~ 32617,
      tract_id %in% c("AGO_131148", "AGO_131128")            ~ 32734,
      TRUE ~ epsg_corr
    )
  ) |> 
  filter(!(iso %in% c("HND", "GTM") & is.na(x_corr))) |>
  dplyr::select(iso, tract_id, nb_plot, plot_no_min, plot_lat, plot_long, x_corr, y_corr, epsg,  epsg_corr)

table(newtract03tmp$nb_plot, newtract03tmp$iso, useNA = "always")
table(newtract03tmp$plot_no_min, newtract03tmp$iso, useNA = "always")



##
## Convert all lat long to utm ##############################################
##

## Filter lat/long data
tract_latlong <- newtract03tmp |>
  filter(!is.na(plot_lat), !is.na(plot_long))

## Apply vectorized loop to different epsg
vec_epsg    <- sort(unique(tract_latlong$epsg_corr))
conv_utm <- map_dfr(.x = vec_epsg, .f = function(x, .tract = tract_latlong){
  
  # x <- vec_epsg[1] ## !!! For testing vectorized loop only
  sf_in <- .tract |>
    filter(epsg_corr == x) |>
    st_as_sf(coords = c("plot_long", "plot_lat"), crs = 4326) |>
    st_transform(crs = x)
  
  ## !!! For testing only  
  # map1 <- sf_country |>
  #     filter(GID_0 %in% unique(sf_in$iso)) |>
  #     ggplot() +
  #     geom_sf(data = sf_in, aes(color = iso)) +
  #     geom_sf_text(aes(label = GID_0), size = 3) +
  #     geom_sf(fill = NA) +
  #     theme_bw() +
  #     theme(legend.position = "none") +
  #     labs(title = paste0("epsg: ", x))
  # print(map1)

    sf_in |>
      bind_cols(utm_x = st_coordinates(sf_in)[,1], utm_y = st_coordinates(sf_in)[,2]) |>
      as_tibble() |>
      dplyr::select(iso, tract_id, utm_x, utm_y)
  
})

## Checks
# check  <- conv_utm |> filter(is.na(utm_x)) |> pull(tract_id)
# check2 <- newtract03tmp |> filter(tract_id %in% check)
# check2



## 
## Get tract mid point instead of first plot starting point #################
##

newtract03tmp2 <- newtract03tmp |>
  left_join(conv_utm, by = c("iso", "tract_id")) |>
  mutate(
    x_corr = if_else(!is.na(utm_x), utm_x, x_corr),
    y_corr = if_else(!is.na(utm_y), utm_y, y_corr),
    tract_x = case_when(
      plot_no_min == 1 ~ x_corr + 250,
      plot_no_min == 2 ~ x_corr + 250,
      plot_no_min == 3 ~ x_corr - 250,
      plot_no_min == 4 ~ x_corr - 250,
      TRUE ~ NA_real_
    ),
    tract_y = case_when(
      plot_no_min == 1 ~ y_corr + 250,
      plot_no_min == 2 ~ y_corr - 250,
      plot_no_min == 3 ~ y_corr - 250,
      plot_no_min == 4 ~ y_corr + 250,
      TRUE ~ NA_real_
    )
  ) |>
  dplyr::select(!c(plot_lat, plot_long, x_corr, y_corr, utm_x, utm_y))

## Checks
newtract03tmp2 |> filter(is.na(tract_x))
newtract03tmp2 |> filter(is.na(tract_y))



## 
## Convert mid point coords back to lat-long ################################
##

## Apply vectorized loop to different epsg
vec_epsg     <- sort(unique(newtract03tmp2$epsg_corr))
conv_latlong <- map_dfr(.x = vec_epsg, .f = function(x, .tract = newtract03tmp2){
  
  # x <- vec_epsg[1] ## !!! For testing vectorized loop only
  sf_in <- .tract |>
    filter(epsg_corr == x) |>
    st_as_sf(coords = c("tract_x", "tract_y"), crs = x) |>
    st_transform(crs = 4326)
  
  ## !!! For testing only  
  # map1 <- sf_country2 |>
  #     filter(GID_0 %in% unique(sf_in$iso)) |>
  #     ggplot() +
  #     geom_sf(data = sf_in, aes(color = iso)) +
  #     geom_sf_text(aes(label = GID_0), size = 3) +
  #     geom_sf(fill = NA, col = "blue") +
  #     theme_bw() +
  #     theme(legend.position = "none") +
  #     labs(title = paste0("epsg: ", x))
  #   print(map1)
  
  sf_in |>
    bind_cols(tract_long = st_coordinates(sf_in)[,1], tract_lat = st_coordinates(sf_in)[,2]) |>
    as_tibble() |>
    dplyr::select(iso, tract_id, tract_long, tract_lat)
  
})


## Add lat/long coords to tract
newtract03tmp3 <- newtract03tmp2 |>
  left_join(conv_latlong, by = c("iso", "tract_id"))


## Checks
gr_coords <- newtract03tmp3 |>
  st_as_sf(coords = c("tract_long", "tract_lat"), crs = 4326) |>
  ggplot() +
  geom_sf(aes(color = iso)) +
  geom_sf(data = sf_country, fill = NA, col = "black") +
  ggrepel::geom_text_repel(
    data = sf_country, 
    aes(label = GID_0, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0
    ) +
  theme_void() +
  theme(legend.position = "none")
print(gr_coords)

## --- Interactive map to track errors --------------------------------------
# sf_tract <- newtract03tmp3 |>
#   st_as_sf(coords = c("tract_long", "tract_lat"), crs = 4326)
# 
# tmap_mode("view")
# tm_shape(sf_country2) + tm_borders() +
# tm_shape(sf_tract) + tm_dots(col = "iso",
#                              palette = "Dark2",
#                              popup.vars = c("iso", "tract_id", "tract_x", "tract_y", "epsg", "epsg_corr"),
#                              popup.format = list(tract_x=list(digits=0), tract_y=list(digits=0))
#                              )


## Remove temp objects
rm(check_coord, vec_epsg, conv_utm, conv_latlong, tract_latlong, newtract03tmp, newtract03tmp2)
