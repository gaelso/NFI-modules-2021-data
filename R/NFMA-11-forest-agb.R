
##
## Overall AGB ##############################################################
##
overall_agb <- newplot10 %>%
  summarise(
    n_plot   = n(),
    mean_agb = mean(plot_agb),
    sd_agb   = sd(plot_agb)
  ) %>%
  mutate(
    ci      = sd_agb / sqrt(n_plot) * 1.96,
    ci_perc = round(ci / mean_agb * 100, 0)
  )

overall_agb



##
## Country level AGB ########################################################
##

country_agb <- newplot10 %>%
  group_by(iso, country) %>%
  summarise(
    n_plot   = n(),
    mean_agb = mean(plot_agb),
    sd_agb   = sd(plot_agb)
  ) %>%
  mutate(
    ci      = sd_agb / sqrt(n_plot) * 1.96,
    ci_perc = round(ci / mean_agb * 100, 0)
  )
country_agb

country_agb %>%
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

lu_agb <- newplot10 %>%
  group_by(iso, country, lu_factor) %>%
  summarise(
    n_plot   = n(),
    mean_agb = mean(plot_agb),
    sd_agb   = sd(plot_agb)
  ) %>%
  mutate(
    ci      = sd_agb / sqrt(n_plot) * 1.96,
    ci_perc = round(ci / mean_agb * 100, 0)
  )
lu_agb

lu_agb %>%
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
  facet_wrap(~lu_factor) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(x = "", y = "Aboveground biomass (ton/ha)", fill = "")

