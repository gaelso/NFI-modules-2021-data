## NFI-modules-2021-learnr
## Gael Sola, FAO



## Harmonize tree data ######################################################

lus02 |> filter(is.na(lus_no)) |> pull(iso) |> unique()
tt <- names(tree01)

sp_codes2 <- sp_codes |> 
  filter(!is.na(sp_code)) |>
  rename(tree_species_code = sp_code) |>
  distinct(country, tree_species_code, .keep_all = T)

tree02tmp <- tree01 |>
  left_join(country_iso, by = "country") |>
  mutate(
    plot_id = case_when(
      !is.na(ID.PLOT)    ~ paste0(iso, "_", ID.PLOT),
      !is.na(Plot.no)    ~ paste0(iso, "_", as.numeric(Tract.ID) * 100 + as.numeric(Plot.no)), ## CRI
      !is.na(parcela)    ~ paste0(iso, "_", as.numeric(unidadmuestreo) * 100 + as.numeric(parcela)), ## HND
      !is.na(ID.PARCELA) ~ paste0(iso, "_", as.numeric(ID.UM) * 100 + as.numeric(ID.PARCELA)), ## NIC
      TRUE ~ NA_character_
    ),
    lus_no = case_when(
      !is.na(X4.LUSNo)  ~ X4.LUSNo,
      iso == "CRI"      ~ ID.SUT, ## CRI, text instead of number, correction below
      !is.na(numerocut) ~ numerocut, ## HND
      !is.na(ID.CUT)    ~ ID.CUT, ## NIC
      TRUE ~ NA_character_
    ),
    tree_no = case_when(
      !is.na(X55.TreeNo) ~ as.numeric(X55.TreeNo),
      !is.na(ID.Tree)    ~ as.numeric(ID.Tree), ## CRI
      !is.na(arbol)      ~ as.numeric(arbol), ## HND
      !is.na(ID.ARBOL)   ~ as.numeric(ID.ARBOL), ## NIC 
      TRUE ~ NA_real_
    ),
    tree_dbh = case_when(
      !is.na(X58.Dbh)    ~ as.numeric(X58.Dbh),
      !is.na(Tree.DapCm) ~ as.numeric(Tree.DapCm), ## CRI
      !is.na(dap)        ~ as.numeric(dap), ## HND
      !is.na(DAP)        ~ as.numeric(DAP), ## NIC
      TRUE ~ NA_real_
    ),
    tree_pom = case_when(
      !is.na(X59.DbhHeight)   ~ as.numeric(X59.DbhHeight),
      !is.na(Tree.HeightDapM) ~ as.numeric(Tree.HeightDapM), ## CRI
      !is.na(hdap)            ~ as.numeric(hdap), ## HND
      !is.na(H.DAP)           ~ as.numeric(H.DAP), ## NIC
      TRUE ~ NA_real_
    ),
    tree_height_top = case_when(
      !is.na(X61.TotalHeight)   ~ as.numeric(X61.TotalHeight),
      !is.na(Tree.TotalHeightM) ~ as.numeric(Tree.TotalHeightM), ## CRI
      !is.na(htotal)            ~ as.numeric(htotal), ## HND
      !is.na(H.TOT)             ~ as.numeric(H.TOT), ## NIC
      TRUE ~ NA_real_
    ),
    tree_height_bole = case_when(
      !is.na(X62.CommercialHeight)  ~ as.numeric(X62.CommercialHeight),
      !is.na(Tree.ComercialHeightM) ~ as.numeric(Tree.ComercialHeightM), ## CRI
      !is.na(hcomercial)            ~ as.numeric(hcomercial), ## HND   
      !is.na(H.COM)                 ~ as.numeric(H.COM), ## NIC
      TRUE ~ NA_real_
    ),
    tree_height_me = case_when(
      !is.na(tipomedicion) ~ tipomedicion, ## HND
      !is.na(M.E)          ~ M.E, ## NIC
      TRUE ~ NA_character_ ## No info for CRI and standard NFMA
    ),
    tree_x = case_when(
      !is.na(X57b.TreePositionWidth) ~ as.numeric(X57b.TreePositionWidth),
      !is.na(Tree.DistX)             ~ as.numeric(Tree.DistX), ## CRI use wrong x and y
      !is.na(distanciax)             ~ as.numeric(distanciax), ## HND use wrong x and y
      !is.na(DX)                     ~ as.numeric(DX), ## NIC use wrong x and y
      TRUE ~ NA_real_
    ),
    tree_y = case_when(
      !is.na(X57a.TreePositionLength) ~ as.numeric(X57a.TreePositionLength),
      !is.na(Tree.DistY)              ~ as.numeric(Tree.DistY), ## CRI use wrong x and y
      !is.na(distanciay)              ~ as.numeric(distanciay), ## HND use wrong x and y
      !is.na(DY)                      ~ as.numeric(DY), ## NIC use wrong x and y
      TRUE ~ NA_real_ 
    ),
    tree_species_code = case_when(
      !is.na(X56.Species) ~ X56.Species,
      iso == "CMR"        ~ X56b.ScientificName, ## CMR store code in scname field
      !is.na(Tree.code)   ~ Tree.code, ## CRI
      !is.na(especie)     ~ especie, ## HND  
      !is.na(ID.ESPECIE)  ~ ID.ESPECIE, ## NIC
      TRUE ~ NA_character_
    ),
    tree_species_name = case_when(
      !is.na(X56b.ScientificName) ~ X56b.ScientificName,
      TRUE ~ NA_character_ ## No value for CRI, HND, NIC
    ),
    tree_health = case_when(
      !is.na(X64.HealthState)        ~ as.numeric(X64.HealthState),
      !is.na(Tree.Health)            ~ as.numeric(Tree.Health), ## CRI 
      !is.na(condicionfitosanitaria) ~ as.numeric(condicionfitosanitaria), ## HND
      !is.na(GRADO.COND.FITO)        ~ as.numeric(GRADO.COND.FITO), ## NIC 
      TRUE ~ NA_real_
    )
  ) |>
  dplyr::select(
    country, iso, plot_id, lus_no, tree_no, tree_dbh, tree_pom, tree_height_top, tree_height_bole, tree_height_me,
    tree_x, tree_y, tree_species_code, tree_species_name, tree_health
    ) |>
  left_join(sp_codes2, by = c("country", "tree_species_code")) |>
  mutate(tree_species_name = if_else(!is.na(sp_name), sp_name, tree_species_name)) |> ## One country has species names only, lost species code
  dplyr::select(-sp_name) |>
  filter(!is.na(lus_no), !is.na(plot_id), !is.na(tree_dbh))

## Add lus_no for CRI
check_lus2 <- check_lus |> 
  dplyr::select(-lus_no) |>
  rename(lus_no = lus_code) |>
  distinct(iso, plot_id, lus_no, .keep_all = T)

tree02 <- tree02tmp |>
  left_join(check_lus2, by = c("iso", "plot_id", "lus_no")) |>
  mutate(lus_no = as.numeric(if_else(!is.na(lus_no_corr), as.character(lus_no_corr), lus_no))) |>
  dplyr::select(-lus_no_corr)

## Checks
summary(tree02$tree_dbh)
summary(tree02$tree_height_top)

tt <- tree02 |> filter(tree_dbh < 10)
table(tt$iso, useNA = "always")

tt <- tree02 |> filter(tree_dbh >= 10, tree_dbh < 20)
table(tt$iso, useNA = "always")

tt <- tree02 |> filter(tree_dbh >= 20)
table(tt$iso, useNA = "always")

# tree02 |>
#   ggplot(aes(x = tree_dbh, y = tree_height_top, color = as.factor(tree_health))) +
#   geom_point() +
#   facet_wrap(~ iso) +
#   theme_bw()

# tree02 |>
#   ggplot(aes(x = tree_x, y = tree_y, color = as.character(lus_no))) +
#   geom_point(alpha = 0.05) +
#   facet_wrap(~iso) +
#   coord_fixed() +
#   theme_bw() +
#   theme(legend.position = "none")

#write_csv(tree, "results/tree.csv")

## Remove temp objects
rm(tree02tmp)

