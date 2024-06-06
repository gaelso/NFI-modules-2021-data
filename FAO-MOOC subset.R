
library(tidyverse)

plot    <- read_csv("results/mock-data/raw_plot.csv", show_col_types = FALSE)
tree    <- read_csv("results/mock-data/raw_tree.csv", show_col_types = FALSE)
species <- read_csv("results/mock-data/raw_species.csv", show_col_types = FALSE)
wdgn    <- read_csv("results/mock-data/raw_wdgn.csv", show_col_types = FALSE)
wdsp    <- read_csv("results/mock-data/raw_wdsp.csv", show_col_types = FALSE)

ano_plot <- read_csv("results/anonymization-codes/ano_plot.csv")

## Select country
sort(unique(ano_plot$iso))
plot_ids <- ano_plot |> filter(iso == "COG") |> pull(raw_id)

nrow(plot_ids)
unique(plot$gez_name)


## Get list of plots with more than 10 trees
plot_wtree <- tree |>
  left_join(select(ano_plot, raw_id, iso), by = join_by(plot_id == raw_id)) |>
  left_join(plot, by = join_by(plot_id)) |>
  group_by(iso, gez_name, lu_code, plot_id) |>
  summarize(count_tree = n(), .groups = "drop")

plot_wtree

table(plot_wtree$iso, plot_wtree$gez_name)


## Select plots
set.seed(10)
plot_sub <- plot_wtree |> 
  filter(
    iso == "COG",
    gez_name == "Tropical rainforest",
    count_tree > 5
    ) |>
  slice_sample(n = 10)

nrow(plot_sub)
nrow(plot_sub) == length(unique(plot_sub$plot_id))


## Select trees
tree_sub <- tree |>
  filter(plot_id %in% plot_sub$plot_id) |>
  left_join(species, by = "sp_id") |>
  left_join(rename(wdsp, wd_species = wd_avg), by = "sp_name") |>
  left_join(rename(wdgn, wd_genus = wd_avg2), by = "genus") |>
  mutate(
    wd = case_when(
      is.na(wd_species) & is.na(wd_genus) ~ 0.511,
      is.na(wd_species) & !is.na(wd_genus) ~ round(wd_genus, 3),
      TRUE ~ round(wd_species, 3)
    )
  )

sort(table(tree_sub$sp_name))
table(tree_sub$wd_species, useNA = "ifany")
table(tree_sub$wd_genus, useNA = "ifany")
table(tree_sub$wd, useNA = "ifany")


## Final tree data
tree_mooc <- tree_sub |>
  select(plot_id, tree_id, tree_dbh, tree_height_top, sp_name, wd)

dir.create("results/MOOC", showWarnings = F)
write_csv(tree_mooc, "results/MOOC/tree_mooc.csv")

ggplot(tree_sub) +
  geom_point(aes(x = tree_dbh, tree_height_top, color = plot_id)) +
  theme_bw()

ggplot(tree_sub) +
  geom_point(aes(x = tree_dbh, tree_height_top, color = plot_id)) +
  facet_wrap(~plot_id) +
  theme_bw()

plot_sub


