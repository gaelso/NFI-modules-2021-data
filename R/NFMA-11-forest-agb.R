
##
## Overall AGB ##############################################################
##
overall_agb <- newplot10 |>
  filter(!is.na(plot_agb)) |>
  summarise(
    n_plot   = n(),
    mean_agb = mean(plot_agb),
    sd_agb   = sd(plot_agb)
  ) |>
  mutate(
    ci      = sd_agb / sqrt(n_plot) * 1.96,
    ci_perc = round(ci / mean_agb * 100, 0)
  )

overall_agb



##
## Country level AGB ########################################################
##

country_agb <- newplot10 |>
  group_by(iso, country) |>
  summarise(
    n_plot   = n(),
    mean_agb = mean(plot_agb),
    sd_agb   = sd(plot_agb)
  ) |>
  mutate(
    ci      = sd_agb / sqrt(n_plot) * 1.96,
    ci_perc = round(ci / mean_agb * 100, 0)
  )
country_agb

country_agb |>
  ggplot(aes(x = iso, y = mean_agb)) +
  geom_col(aes(fill = iso), position = position_dodge(0.9), color = 'black') +
  scale_fill_viridis_d() +
  geom_errorbar(
    aes(ymin = mean_agb - ci, ymax = mean_agb + ci),
    position = position_dodge(.9),
    width = 0.5,
    color = 'black'
  ) +
  geom_text(
    aes(y = mean_agb + ci + 10, label = n_plot),
    position = position_dodge(.9), size = 3
  ) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(x = "", y = "Aboveground biomass (ton/ha)", fill = "")
  


##
## Land use AGB #############################################################
##

lu_agb <- newplot10 |>
  group_by(iso, country, lu_harmo2) |>
  summarise(
    n_plot   = n(),
    mean_agb = mean(plot_agb),
    sd_agb   = sd(plot_agb)
  ) |>
  mutate(
    ci      = sd_agb / sqrt(n_plot) * 1.96,
    ci_perc = round(ci / mean_agb * 100, 0)
  )
lu_agb

lu_agb |>
  ggplot(aes(x = iso, y = mean_agb)) +
  geom_col(aes(fill = iso), position = position_dodge(0.9), color = 'black') +
  scale_fill_viridis_d() +
  geom_errorbar(
    aes(ymin = mean_agb - ci, ymax = mean_agb + ci),
    position = position_dodge(.9),
    width = 0.5,
    color = 'black'
  ) +
  geom_text(
    aes(y = mean_agb + ci + 10, label = n_plot),
    position = position_dodge(.9), size = 3
  ) +
  facet_wrap(~lu_harmo2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(x = "", y = "Aboveground biomass (ton/ha)", fill = "")


## Land use / GEZ
lu_agb2 <- newplot10 |>
  filter(!is.na(plot_agb)) |>
  group_by(lu_harmo2) |>
  summarise(
    n_plot   = n(),
    mean_agb = mean(plot_agb),
    sd_agb   = sd(plot_agb)
  ) |>
  mutate(
    ci      = sd_agb / sqrt(n_plot) * 1.96,
    ci_perc = round(ci / mean_agb * 100, 0)
  )
lu_agb2

lu_agb2 |>
  ggplot(aes(x = lu_harmo2, y = mean_agb)) +
  geom_col(aes(fill = lu_harmo2), position = position_dodge(0.9), color = 'black') +
  scale_fill_viridis_d() +
  geom_errorbar(
    aes(ymin = mean_agb - ci, ymax = mean_agb + ci),
    position = position_dodge(.9),
    width = 0.5,
    color = 'black'
  ) +
  geom_text(
    aes(y = mean_agb + ci + 10, label = n_plot),
    position = position_dodge(.9), size = 3
  ) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(x = "", y = "Aboveground biomass (ton/ha)", fill = "")


gez_agb <- newplot10 |>
  filter(!is.na(plot_agb)) |>
  group_by(iso, country, gez_name) |>
  summarise(
    n_plot   = n(),
    mean_agb = mean(plot_agb),
    sd_agb   = sd(plot_agb)
  ) |>
  mutate(
    ci      = sd_agb / sqrt(n_plot) * 1.96,
    ci_perc = round(ci / mean_agb * 100, 0)
  )
gez_agb

gez_agb |>
  ggplot(aes(x = iso, y = mean_agb)) +
  geom_col(aes(fill = iso), position = position_dodge(0.9), color = 'black') +
  scale_fill_viridis_d() +
  geom_errorbar(
    aes(ymin = mean_agb - ci, ymax = mean_agb + ci),
    position = position_dodge(.9),
    width = 0.5,
    color = 'black'
  ) +
  geom_text(
    aes(y = mean_agb + ci + 10, label = n_plot),
    position = position_dodge(.9), size = 3
  ) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_wrap(~gez_name) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(x = "", y = "Aboveground biomass (ton/ha)", fill = "")



gez_agb2 <- newplot10 |>
  filter(!is.na(plot_agb)) |>
  group_by(gez_name) |>
  summarise(
    n_plot   = n(),
    mean_agb = mean(plot_agb),
    sd_agb   = sd(plot_agb)
  ) |>
  mutate(
    ci      = sd_agb / sqrt(n_plot) * 1.96,
    ci_perc = round(ci / mean_agb * 100, 0)
  )
gez_agb2

gez_agb2 |>
  ggplot(aes(x = gez_name, y = mean_agb)) +
  geom_col(aes(fill = gez_name), position = position_dodge(0.9), color = 'black') +
  scale_fill_viridis_d() +
  geom_errorbar(
    aes(ymin = mean_agb - ci, ymax = mean_agb + ci),
    position = position_dodge(.9),
    width = 0.5,
    color = 'black'
  ) +
  geom_text(
    aes(y = mean_agb + ci + 10, label = n_plot),
    position = position_dodge(.9), size = 3
  ) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(x = "", y = "Aboveground biomass (ton/ha)", fill = "")
