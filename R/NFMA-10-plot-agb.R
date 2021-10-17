## NFI-modules-2021-learnr
## Gael Sola, FAO

table(newplot04$iso)

newplot10tmp <- tree09 %>%
  group_by(iso, tract_id, plot_id) %>%
  summarise(
    plot_ba  = sum((tree_dbh / 200)^2 * pi * scale_factor),
    plot_agb = sum(tree_agb * scale_factor)
    ) %>%
  ungroup() %>%
  left_join(newplot04, by = c("iso", "tract_id", "plot_id")) %>%
  filter(plot_no == plot_no_min)


newplot10tmp %>%
  ggplot(aes(x = plot_ba, y = plot_agb)) +
  geom_point(aes(color = iso)) +
  theme_bw()

newplot10tmp %>%
  ggplot(aes(x = plot_ba, y = plot_agb)) +
  geom_point(aes(fill = gez_name), shape = 21, alpha = 50) +
  scale_fill_viridis_d(direction = -1) +
  facet_grid(iso ~ gez_name) +
  theme_bw() +
  theme(legend.position = "none")

newplot10tmp %>%
  ggplot(aes(x = plot_ba, y = plot_agb)) +
  geom_point(aes(fill = gez_name), shape = 21, alpha = 50) +
  scale_fill_viridis_d(direction = -1) +
  facet_wrap(~ lu_harmo2) +
  theme_bw() +
  theme(legend.position = "none")

newplot10tmp %>%
  mutate(lu_factor = forcats::fct_reorder(lu_harmo2, lu_harmo_num)) %>%
  ggplot(aes(x = plot_ba, y = plot_agb)) +
  geom_point(aes(fill = lu_factor), shape = 21, alpha = 50) +
  scale_fill_viridis_d(direction = -1) +
  facet_wrap(~ lu_harmo2) +
  theme_bw() +
  theme(legend.position = "none")


newplot10tmp %>%
  mutate(lu_factor = forcats::fct_reorder(lu_harmo2, lu_harmo_num)) %>%
  filter(plot_ba < 80) %>%
  ggplot(aes(x = iso, y = plot_agb)) +
  geom_boxplot(aes(fill = lu_harmo1), shape = 21, alpha = 50) +
  scale_fill_viridis_d(direction = -1) +
  facet_wrap(~lu_factor) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw()

table(newplot10tmp$iso)
table(newplot10tmp$lu_harmo2, newplot10tmp$iso)


## Filter outliers, countries with not enough plots and other land uses
newplot10 <- newplot10tmp %>%
  mutate(lu_factor = forcats::fct_reorder(lu_harmo2, lu_harmo_num)) %>%
  filter(plot_ba < 80, iso != "COM", lu_harmo2 != "Other")

gr_plot_agb <- newplot10 %>%
  ggplot(aes(x = iso, y = plot_agb)) +
  geom_boxplot(aes(fill = lu_harmo1), shape = 21, alpha = 50) +
  scale_fill_viridis_d(direction = -1) +
  facet_wrap(~lu_factor) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw()
print(gr_plot_agb)

gr_plot_agb2 <- newplot10 %>%
  ggplot(aes(x = plot_ba, y = plot_agb)) +
  geom_point(aes(fill = gez_name), shape = 21, alpha = 50) +
  scale_fill_viridis_d(direction = -1) +
  facet_grid(iso ~ lu_factor) +
  theme_bw() +
  theme(legend.position = "none")
print(gr_plot_agb2)
