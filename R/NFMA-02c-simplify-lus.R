## NFI-modules-2021-learnr
## Gael Sola, FAO


## Land use subplots to become main plots ###################################

## Check column names
# tt <- names(lus_init)
# lus_init |> filter(!is.na(ID.LUS)) |> pull(country) |> unique()  ## Missing CRI, HND, NIC
# lus_init |> filter(!is.na(X81a.SubPlotWidth)) |> pull(country) |> unique()  ## Missing CRI, HND, NIC
# lus_init |> filter(!is.na(X80.LandUse)) |> pull(country) |> unique()  ## Missing CRI, HND, NIC


lus02tmp <- lus01 |>
  left_join(country_iso, by = "country") |>
  mutate(
    plot_id = case_when(
      !is.na(ID.PLOT)    ~ paste0(iso, "_", ID.PLOT),
      !is.na(Plot.no)    ~ paste0(iso, "_", as.numeric(Tract.ID) * 100 + as.numeric(Plot.no)), ## CRI
      !is.na(parcela)    ~ paste0(iso, "_", as.numeric(unidadmuestreo) * 100 + as.numeric(parcela)), ## HND
      !is.na(id.parcela) ~ paste0(iso, "_", as.numeric(id.um) * 100 + as.numeric(id.parcela)), ## NIC
      TRUE ~ NA_character_
    ),
    lus_id = case_when(
      !is.na(ID.LUS) ~ paste0(iso, "_", ID.LUS),
      !is.na(ID.SUT) ~ paste0(plot_id, "_", ID.SUT), ## CRI
      !is.na(numerocut) ~ paste0(plot_id, "_", numerocut), ## HND
      !is.na(id.sut)   ~ paste0(plot_id, "_ ", id.sut), ## NIC
      TRUE ~ NA_character_
    ),
    lus_no = case_when(
      !is.na(X4.LUSNo)  ~ as.numeric(X4.LUSNo),
      iso == "CRI"      ~ as.numeric(ID), ## CRI placeholder, correction below
      !is.na(numerocut) ~ as.numeric(numerocut), ## HND
      !is.na(id.sut)    ~ as.numeric(id.sut), ## NIC
      TRUE ~ NA_real_
    ),
    plot_width = case_when(
      !is.na(X81a.SubPlotWidth) ~ as.numeric(X81a.SubPlotWidth), 
      !is.na(SubPlot.Width)     ~ as.numeric(SubPlot.Width), ## CRI
      !is.na(anchocut)          ~ as.numeric(anchocut), ## HND
      !is.na(Area)              ~ 20, ## NIC correcting later based on max tree position  
      TRUE ~ NA_real_
    ),
    plot_length = case_when(
      !is.na(X81b.SubPlotLenght) ~ as.numeric(X81b.SubPlotLenght),
      !is.na(SubPlot.Length)     ~ as.numeric(SubPlot.Length), ## CRI
      !is.na(longitudcut)        ~ as.numeric(longitudcut), ## HND
      !is.na(Area)              ~ round(as.numeric(Area) * 10000 / 20, 0), ## NIC correcting later based on max tree position  
      TRUE ~ NA_real_
    ),
    lus_code = case_when(
      !is.na(X80.LandUse) ~ X80.LandUse,
      !is.na(ID.SUT)      ~ ID.SUT, ## CRI
      !is.na(idcut)       ~ idcut, ## HND
      !is.na(id.cut.tb)   ~ id.cut.tb, ## NIC
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::select(iso, country, plot_id, lus_id, lus_no, plot_width, plot_length, lus_code) |>
  left_join(lu_codes |> rename(lus_code = id, lus = lu), by = c("country", "lus_code")) |>
  mutate(lus = stringi::stri_trans_general(lus, "Latin-ASCII") |> str_to_lower()) |>
  distinct()

## Checks
table(lus02tmp$lus, lus02tmp$iso, useNA = "always")
# table(lus$lus_no, lus$iso, useNA = "always")

## Correct lus_no for CRI
lus02tmp_sel <- lus02tmp |> dplyr::select(iso, plot_id, lus_code, lus_no)

check_lus <- lus02tmp |> 
  filter(iso == "CRI") |>
  group_by(iso, plot_id) |>
  summarise(lus_no_min = min(lus_no)) |>
  left_join(lus02tmp_sel, by = c("iso", "plot_id")) |>
  mutate(lus_no_corr = lus_no - lus_no_min + 1) |>
  dplyr::select(iso, plot_id, lus_code, lus_no, lus_no_corr)

## Apply filters to remove wrong plot size and correct lus_no for CRI
lus02 <- lus02tmp |>
  left_join(check_lus, by = c("iso", "plot_id", "lus_code", "lus_no")) |>
  mutate(lus_no = if_else(!is.na(lus_no_corr), lus_no_corr, lus_no)) |>
  filter(plot_length != 596, plot_length != 209, plot_width != 24) |>
  dplyr::select(-lus_no_corr)

## Checks
length(unique(paste0(lus02tmp$plot_id, "_", lus02tmp$lus_no))) == nrow(lus02tmp)
length(unique(paste0(lus02$plot_id, "_", lus02$lus_no))) == nrow(lus02)

table(lus02$lus_no, lus02$iso, useNA = "always")
table(lus02$lus, lus02$iso, useNA = "always")
 
plot_length <- lus02 |>
  group_by(iso) |>
  summarise(max_plotlength = max(plot_length, na.rm = T))
plot_length

plot_width <- lus02 |>
  group_by(iso) |>
  summarise(max_plotwidth = max(plot_width, na.rm = T))
plot_width

## Remove temp objects
rm(lus02tmp, lus02tmp_sel)

