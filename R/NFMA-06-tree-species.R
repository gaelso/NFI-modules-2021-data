## NFI-modules-2021-learnr
## Gael Sola, FAO


## Make the species list and submit to the species validation tool from Lauri Vesa and all.


## Clean species names with a table containing genus and epithet intraspecies names
tree_specieslist <- tibble(input = sort(unique(tree02$tree_species_name)))
write_csv(tree_specieslist |> rename(scientific_name = input), "data/NFMA/species_cor/NFMA_species_mess.csv")
set.seed(11)
write_csv(tree_specieslist |> rename(scientific_name = input) |> slice_sample(n = 100), "data/NFMA/species_cor/NFMA_species_mess100.csv")


## Upload and join corrected species
species_cor <- read_csv("data/NFMA/species_cor/NFMA_species_mess-results.csv")
tree06      <- tree05 |> left_join(species_cor, by = c("tree_species_name" = "scientific_name"))


## Checks
table(tree06$status, useNA = "always")
table(tree06$iucn_label, useNA = "always")
table(tree06$gts_match, useNA = "always")

tt <- tree06 |> filter(is.na(iucn_label))


## Remove temp objects
rm(tree_specieslist, species_cor)

