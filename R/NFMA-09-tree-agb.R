## NFI-modules-2021-learnr
## Gael Sola, FAO

tree09 <- tree08 |>
  mutate(
    tree_agb = 0.0673 * (tree_wd * tree_dbh^2 * tree_height_cor)^0.976 /10^3
  )

tree09 |>
  ggplot(aes(x = tree_dbh, y = tree_agb)) +
  geom_point() +
  facet_wrap(~iso) +
  theme_bw()

tree09 |>
  ggplot(aes(x = tree_dbh, y = tree_agb)) +
  geom_point() +
  facet_wrap(~gez_name) +
  theme_bw()

tree09 |>
  ggplot(aes(x = tree_dbh, y = tree_agb, color = iso)) +
  scale_color_viridis_d() +
  geom_point() +
  facet_wrap(~gez_name) +
  theme_bw()

gr_out <- tree09 |>
  filter(tree_dbh < 200, gez_name != "Tropical mountain system") |>
  ggplot(aes(x = tree_dbh, y = tree_agb, fill = gez_name)) +
  scale_fill_viridis_d(direction = -1) +
  geom_point(pch = 21, alpha = 0.5) +
  facet_grid(iso ~ gez_name) +
  theme_bw()

print(gr_out)
