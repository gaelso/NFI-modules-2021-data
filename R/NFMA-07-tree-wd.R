## NFI-modules-2021-learnr
## Gael Sola, FAO

## Download Wood Density databases from the internet
## Refs: 
##    Vieilledent, G., Fischer, F. J., Chave, J., Guibal, D., Langbour, P., and Gérard, J.. 2018. 
##    New formula and conversion factor to compute basic wood density of tree species using a global wood technology database. 
##    American Journal of Botany 105( 10): 1653– 1661. 
##    https://agritrop.cirad.fr/589166/1/Vieilledent2018-AJB.pdf
##
##    Zanne, Amy E. et al. (2009), 
##    Data from: Towards a worldwide wood economics spectrum, 
##    Dryad, Dataset.
##    https://doi.org/10.5061/dryad.234 
##
##    Chave J, Coomes D, Jansen S, Lewis SL, Swenson NG, Zanne AE. 
##    Towards a worldwide wood economics spectrum. Ecol Lett. 2009 Apr;12(4):351-66. 
##    doi: 10.1111/j.1461-0248.2009.01285.x. Epub 2009 Feb 20. PMID: 19243406.


##
## Load data ################################################################
##

## Abort using Cirad list, issues with duplicates
# cwdd_species <- unique(sort(cwdd$species_name))
# gwd_species  <- unique(sort(gwd$species_name))
# gwd_add      <- setdiff(gwd_species, cwdd_species) 
# 
# length(cwdd_species)
# length(gwd_species)
# length(gwd_add)
# 
# ## Combine 
# wd_all <- gwd |> 
#   filter(species_name %in% gwd_add) |>
#   bind_rows(cwdd, .) |>
#   arrange(species_name)
wd_all <- gwd


## Send species to correct
wd_specieslist <- tibble(input = sort(unique(wd_all$species_name)))
write_csv(wd_specieslist |> rename(scientific_name = input), "data/WD/WD_species.csv")

## Get corrected species
wd_speciescorr <- read_csv("data/WD/WD_species-results.csv") |>
  dplyr::select(species_name = scientific_name, accepted_name)

## Get average per species and genus
wd_corr <- wd_all |> 
  filter(species_name %in% wd_speciescorr$accepted_name) |>
  rename(accepted_name = species_name)

length(unique(wd_corr$accepted_name))

## Species level averages
wd_species <- wd_corr |>
  group_by(accepted_name, source) |>
  summarise(
    count  = n(),
    wd_avg = mean(wd),
    wd_sd  = sd(wd)
    ) |>
  ungroup()

table(wd_species$source)

## Genus level average of species average (each species has weight 1)
wd_genus <- wd_species |>
  mutate(accepted_genus = word(accepted_name)) |>
  group_by(accepted_genus) |>
  summarise(
    count  = n(),
    wd_avg2 = mean(wd_avg),
    wd_sd2  = sd(wd_avg)
  ) |>
  ungroup()


## Join to tree
tree07 <- tree06 |>
  left_join(wd_species |> select(accepted_name, wd_avg), by = "accepted_name") |>
  left_join(wd_genus |> select(accepted_genus, wd_avg2), by = "accepted_genus") |>
  mutate(
    tree_wd = case_when(
      !is.na(wd_avg) ~ wd_avg,
      !is.na(wd_avg2) ~ wd_avg2,
      iso %in% c("AGO", "CMR", "COM", "COG") ~ 0.5,
      iso %in% c("GMB", "KEN", "ZMB")        ~ 0.5,
      iso %in% c("BGD", "KGZ", "LBN", "PHL") ~ 0.57,
      iso %in% c("CRI", "GTM", "HND", "NIC") ~ 0.6,
      TRUE ~ NA_real_
    ),
    wd_level = case_when(
      !is.na(wd_avg)                         ~ "species",
      !is.na(wd_avg2)                        ~ "genus",
      iso %in% c("AGO", "CMR", "COM", "COG") ~ "region1",
      iso %in% c("GMB", "KEN", "ZMB")        ~ "region1",
      iso %in% c("BGD", "KGZ", "LBN", "PHL") ~ "region2",
      iso %in% c("CRI", "GTM", "HND", "NIC") ~ "region3",
      TRUE ~ NA_character_
    )
  ) |>
  select(-wd_avg, -wd_avg2)

## Checks
table(tree07$wd_level, useNA = "always")

## Remove tmp object
#rm(cwdd_species, gwd_species, gwd_add, wd_all, wd_corr, wd_specieslist, wd_speciescorr)
rm(wd_all, wd_corr, wd_specieslist, wd_speciescorr)

