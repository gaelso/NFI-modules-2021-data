## NFI-modules-2021-learnr
## Gael Sola, FAO


## Harmonize land uses and land uses codes ######################################

## Check land uses
tt <- newplot02 |>
  filter(str_detect(string = lus, pattern = "foret|forest|bosque|latifoliado|manglar|pino"))

tt_forest <- tt |> pull(lus) |> unique() |> sort()
tt_forest
tt_nonforest <- setdiff(unique(newplot02$lus), tt_forest) |> sort()
tt_nonforest

tt <- newplot02 |> filter(is.na(lus))
table(tt$iso)
tt <- newplot02 |> filter(!is.na(lus))
table(tt$iso)

## Land use remade categories
lu_harmo_class <- tibble(
  lu_harmo      = c("Evergreen", "Mixed Deciduous", "Deciduous", "Plantation", "Mangrove", "Other woodland"),
  lu_harmo_num  = 1:6,
  lu_harmo_code = c("EV", "MD", "DE", "PL", "MG", "WL")
)


## Create plots and LCC objects for forest
newplot04tmp1 <- newplot02 |>
  dplyr::select(iso, country, tract_id, plot_id, plot_no, plot_width, plot_length, lus_no, lus_code, lus) |>
  left_join(newtract03, by = c("iso", "tract_id")) |>
  filter(str_detect(string = lus, pattern = "foret|forest|bosque|latif|manglar|pino")) |>
  mutate(
    lu_mg = if_else(str_detect(lus, "manglar|mangrove"), 1, 0),
    lu_pl = if_else(str_detect(lus, "plantation|plantacion"), 1, 0),
    #lu_bl = if_else(str_detect(lus, "broad-leaved|broadleaved|latif|evergreen|deciduous|gallery|mixed|mixto|decidue|sempervirente"), 1, 0),
    lu_co = if_else(str_detect(lus, "conif|pino|picea|juniperus|mixed|mixto"), 1, 0),
    lu_ev = if_else(str_detect(lus, "evergreen|sempervirente|semi-deciduous|semi-evergreen"), 1, 0),
    lu_de = if_else(str_detect(lus, "deciduous|decidous|decidue|semi-evergreen"), 1, 0),
    lu_pr = if_else(str_detect(lus, "primary|primario|primaire"), 1, 0), 
    lu_se = if_else(str_detect(lus, "secondary|secundario|secondaire"), 1, 0),
    lu_ds = if_else(str_detect(lus, "dense|denso|closed"), 1, 0),
    lu_op = if_else(str_detect(lus, "open|ralo"), 1, 0),
    lu_evde = lu_ev + lu_de,
    lu_prse = lu_pr + lu_se,
    lu_dsop = lu_ds + lu_op,
    lu_harmo1 = case_when(
      lu_pl == 0 & lu_mg == 1              ~ "Mangrove",
      lu_pl == 0 & lu_ev == 1 & lu_de == 0 ~ "Evergreen",
      lu_pl == 0 & lu_ev == 1 & lu_de == 1 ~ "Mixed Deciduous",
      lu_pl == 0 & lu_ev == 0 & lu_de == 1 ~ "Deciduous",
      lu_pl == 1 & lu_co == 0              ~ "Plantation",
      TRUE ~ "Other"
    ),
    lu_harmo2 = case_when(
      lu_harmo1 != "Other"                           ~ lu_harmo1,
      lu_pl == 0 & lu_co == 0 & gez_abbrev == "TAr"  ~ "Evergreen",
      lu_pl == 0 & lu_co == 0 & gez_abbrev == "TAwa" ~ "Mixed Deciduous",
      lu_pl == 0 & lu_co == 0 & gez_abbrev == "TAwb" ~ "Deciduous",
      lu_pl == 0 & lu_co == 0 & gez_abbrev == "TM"   ~ "Mixed Deciduous",
      TRUE ~ "Other"
    )
  ) |>
  left_join(lu_harmo_class, by = c("lu_harmo2" = "lu_harmo"))


## Create plots and LCC objects for woodlands
newplot04tmp2 <- newplot02 |>
  dplyr::select(iso, country, tract_id, plot_id, plot_no, plot_width, plot_length, lus_no, lus_code, lus) |>
  left_join(newtract03, by = c("iso", "tract_id")) |>
  filter(str_detect(string = lus, pattern = "foret|forest|bosque|latif|manglar|pino", negate = T)) |>
  filter(str_detect(string = lus, pattern = "wood|con arboles|with trees")) |>
  mutate(
    lu_mg = 0,
    lu_pl = 0,
    lu_co = 0,
    lu_ev = 0,
    lu_de = 0,
    lu_pr = 0, 
    lu_se = 0,
    lu_ds = 0,
    lu_op = 0,
    lu_evde = lu_ev + lu_de,
    lu_prse = lu_pr + lu_se,
    lu_dsop = lu_ds + lu_op,
    lu_harmo1 = "Other woodland",
    lu_harmo2 = "Other woodland"
  ) |>
  left_join(lu_harmo_class, by = c("lu_harmo2" = "lu_harmo"))


## Combine forest and woodland plots
newplot04 <- bind_rows(newplot04tmp1, newplot04tmp2)

## Checks
table(newplot04$lu_harmo2)

tt <- newplot04 |> filter(lu_harmo2 == "Other")
table(tt$lus, tt$gez_code)

sf_gez2 |> as_tibble() |> dplyr::select(gez_code, gez_name) |> distinct() |> arrange(gez_code)


tt <- newplot04 |> filter(is.na(gez_name))
tt

table(newplot04$iso)
length(unique(newplot04$plot_id)) == length(unique(paste0(newplot04$plot_id, "_", newplot04$lus_no)))



## Remove temp objects
rm(tt_forest, tt_nonforest, newplot04tmp1, newplot04tmp2)
