## NFI-modules-2021-learnr
## Gael Sola, FAO


## Filter trees and reassign tree location for circular plots ###############

plot_radius    <- 20 ## must be <= 20, alt 17.84
subplot_radius <- 5  ## must be <= 5, alt 3.99


newplot05tmp <- newplot04 %>%
  select(iso, tract_id, plot_id, plot_no, lus_no, lus, gez_code, gez_name, gez_abbrev, envir_stress)


## Check lus
tree05tmp <- tree02 %>% 
  left_join(newplot05tmp, by = c("iso", "plot_id", "lus_no")) %>%
  filter(!is.na(lus)) %>%
  mutate(
    x_tmp  = if_else(iso %in% c("CRI", "HND", "NIC"), tree_y, tree_x),
    tree_y = if_else(iso %in% c("CRI", "HND", "NIC"), tree_x, tree_y),
    tree_x = x_tmp,
    tree_xabs  = case_when(
      plot_no == 1 ~ tree_x - 250,
      plot_no == 2 ~ tree_y - 250,
      plot_no == 3 ~ - tree_x + 250,
      plot_no == 4 ~ - tree_y + 250
    ),
    tree_yabs = case_when(
      plot_no == 1 ~ tree_y - 250,
      plot_no == 2 ~ tree_x + 250,
      plot_no == 3 ~ -tree_y + 250,
      plot_no == 4 ~ -tree_x -250
    )
  ) %>%
  select(-x_tmp)


## Checks
tree05tmp %>% filter(is.na(gez_name))

# tree05tmp %>%
#   ggplot(aes(x = tree_x, y = tree_y, color = as.character(plot_no))) +
#   geom_point(alpha = 0.05) +
#   facet_wrap(~iso) +
#   xlim(-20, 20) +
#   theme_bw()
# 
# tree05tmp %>%
#   ggplot(aes(x = tree_xabs, y = tree_yabs, color = as.character(plot_no))) +
#   geom_point(alpha = 0.1) +
#   facet_wrap(~iso) +
#   theme_bw() +
#   coord_fixed()



##
## Make circular plots ######################################################
##

## --- Big Trees (dbh >= 20 cm)
bigtree <- tree05tmp %>%
  mutate(
    tree_xsq     = if_else(tree_y <= 40, tree_x - 10, tree_x + 10),
    tree_ysq     = if_else(tree_y <= 40, tree_y - 20, tree_y - 60),
    tree_dist    = round(sqrt(tree_xsq^2 + tree_ysq^2), 1),
    tree_angle   = atan2(tree_ysq, tree_xsq),
    tree_degree  = (pi/2 - tree_angle) * 180 / pi,
    tree_azimuth = round(if_else(tree_degree < 0 , 360 + tree_degree, tree_degree)),
    tree_size    = "dbh >= 20"
  ) %>%
  filter(tree_dbh >= 20)

## Checks
summary(bigtree$tree_dist)
summary(bigtree$tree_angle)
summary(bigtree$tree_degree)
summary(bigtree$tree_azimuth)

# bigtree %>%
#   ggplot(aes(x = tree_x, y = tree_y, color = as.character(plot_no))) +
#   geom_point(alpha = 0.1) +
#   geom_point(data = bigtree %>% filter(tree_dist <= plot_radius), col = "red") +
#   facet_wrap(~iso, nrow = 1) +
#   theme_bw() +
#   theme(
#     legend.position = "none",
#     axis.text.x = element_blank()
#     ) +
#   coord_fixed()
# 
# bigtree %>%
#   ggplot(aes(x = tree_xsq, y = tree_ysq, color = as.character(plot_no))) +
#   geom_point(alpha = 0.1) +
#   geom_point(data = bigtree %>% filter(tree_dist <= plot_radius), col = "red") +
#   facet_wrap(~iso, nrow = 1) +
#   theme_bw() +
#   coord_fixed()

# tt <- bigtree %>%
#   filter(plot_id == "AGO_1303622") %>%
#   filter(tree_dist <= 20) 
# 
# tt %>%
#   ggplot(aes(x = tree_xsq, y = tree_ysq, color = tree_azimuth)) +
#   geom_text(aes(label = tree_no)) +
#   theme_bw() +
#   xlim(-20, 20) +
#   ylim(-20, 20)
#   
# tt %>%
#   ggplot(aes(x = tree_angle, y = tree_dist, color = tree_azimuth)) +
#   scale_x_continuous(
#     expand = c(0, 0), 
#     limits = c(-pi, pi),
#     breaks = c(-2:2) * pi/2,
#     labels = c("W", "S", "E", "N", "W")
#     ) +
#   geom_text(aes(label = tree_no)) +
#   theme_bw() +
#   ylim(0, 20) +
#   coord_polar(theta = "x", start = pi/2, direction = -1)
# 
# bigtree %>%
#   filter(tree_dist <= plot_radius) %>%
#   ggplot(aes(x = tree_angle, y = tree_dist, color = tree_azimuth)) +
#   scale_x_continuous(
#     expand = c(0, 0), 
#     limits = c(-pi, pi),
#     breaks = c(-2:2) * pi/2,
#     labels = c("W", "S", "E", "N", "W")
#   ) +
#   geom_point() +
#   theme_bw() +
#   ylim(0, 20) +
#   coord_polar(theta = "x", start = pi/2, direction = -1)


## --- Small Trees (10 <= dbh < 20)
smalltree <- tree05tmp %>% 
  filter(tree_dbh >= 10, tree_dbh < 20, tree_y <= 10, tree_x >= -5, tree_x <= 5) %>%
  mutate(
    tree_xsq = tree_x,
    tree_ysq = tree_y - 5,
    tree_dist    = round(sqrt(tree_xsq^2 + tree_ysq^2), 1),
    tree_angle   = atan2(tree_ysq, tree_xsq),
    tree_degree  = (pi/2 - tree_angle) * 180 / pi,
    tree_azimuth = round(if_else(tree_degree < 0, 360 + tree_degree, tree_degree)),
    tree_size    = "10 <= dbh < 20"
    )

# smalltree %>%
#   ggplot(aes(x = tree_xsq, y = tree_ysq, color = as.character(plot_no))) +
#   geom_point(alpha = 0.1) +
#   geom_point(data = smalltree %>% filter(tree_dist <= subplot_radius), col = "red") +
#   facet_wrap(~iso) +
#   theme_bw() +
#   coord_fixed()
# 
# smalltree %>%
#   filter(tree_dist <= 5) %>%
#   ggplot(aes(x = tree_angle, y = tree_dist, color = tree_azimuth)) +
#   scale_x_continuous(
#     expand = c(0, 0), 
#     limits = c(-pi, pi),
#     breaks = c(-2:2) * pi/2,
#     labels = c("W", "S", "E", "N", "W")
#   ) +
#   geom_point() +
#   theme_bw() +
#   ylim(0, 20) +
#   coord_polar(theta = "x", start = pi/2, direction = -1)


## Group small and big trees
tree05 <- bigtree %>%
  filter(tree_dist <= plot_radius) %>%
  bind_rows(smalltree %>% filter(tree_dist <= subplot_radius)) %>%
  mutate(
    plot_area    = round(if_else(tree_dbh < 20, pi * subplot_radius^2, pi * plot_radius^2) / 10000, 3),
    scale_factor = round(1 / plot_area)
    )

## Checks
gr_treepos <- tree05 %>%
  ggplot(aes(x = tree_angle, y = tree_dist)) +
  geom_point(aes(color = tree_azimuth), alpha = 0.4) +
  geom_point(data = tree05 %>% filter(tree_dbh <20), aes(fill = tree_azimuth), shape = 21) +
  scale_color_viridis_c(direction = -1) +
  scale_fill_distiller(type = "seq", direction = 1, palette = "Greys") +
  scale_x_continuous(
    expand = c(0, 0), 
    limits = c(-pi, pi),
    breaks = c(-2:2) * pi/2,
    labels = c("W", "S", "E", "N", "W")
  ) +
  geom_hline(aes(yintercept = plot_radius), col = "darkred") +
  geom_hline(aes(yintercept = subplot_radius), col = "darkred") +
  theme_bw() +
  ylim(0, 20) +
  coord_polar(theta = "x", start = pi/2, direction = -1) +
  labs(x = "", y = "Distance (m)", fill = "Azimuth", color = "")
  
print(gr_treepos)


gr_treepos2 <- tree05 %>%
  filter(plot_id == "AGO_1303622") %>%
  ggplot(aes(x = tree_angle, y = tree_dist, color = tree_azimuth, size = tree_dbh)) +
  geom_point(alpha = 0.5) +
  scale_size(range = c(min(tree05 %>% filter(plot_id == "AGO_1303622") %>% pull(tree_dbh) / 20), max(tree05 %>% filter(plot_id == "AGO_1303622") %>% pull(tree_dbh) / 20))) +
  scale_color_viridis_c(direction = -1) +
  #scale_shape_manual(values = c(8, 19)) +
  scale_x_continuous(
    expand = c(0, 0), 
    limits = c(-pi, pi),
    breaks = c(-2:2) * pi/2,
    labels = c("W", "S", "E", "N", "W")
  ) +
  geom_hline(aes(yintercept = plot_radius), col = "darkred") +
  geom_hline(aes(yintercept = subplot_radius), col = "darkred") +
  theme_bw() +
  ylim(0, 20) +
  coord_polar(theta = "x", start = pi/2, direction = -1) +
  labs(x = "", y = "Distance (m)", color = "Azimuth", size = "DBH class")
print(gr_treepos2)

## Remove temp objects
rm(newplot05tmp, tree05tmp, bigtree, smalltree)


## Correct different subplots for different countries <- all compatible with < 20 nested subplot

## AGO: S1 radius 3.99 for dbh < 10 and h < 1.3, S2 20x10, 10 <= DBH < 20, S3 250x20 DBH >= 20
## CMR: S1 radius 3.99 for dbh < 10 and h < 1.3, S2 20x10, 10 <= DBH < 20, S3 250x20 DBH >= 20
## COG: S1 radius 3.99 for dbh < 10 and h < 1.3, S2 20x10, 10 <= DBH < 20, S3 250x20 DBH >= 20
## COM: S1 radius 3.99 for dbh < 10 and h < 1.3, S2 20x10, 10 <= DBH < 20, S3 100x20 DBH >= 20
## GMB: S1 radius 3.99 for dbh < 10 and h < 1.3, S2 20x10, 10 <= DBH < 20, S3 250x20 DBH >= 20
## KEN: S1 radius 3.99 for dbh < 10 and h < 1.3, S2 20x10, 10 <= DBH < 20, S3 250x20 DBH >= 20
## ZMB: S1 radius 3.99 for dbh <  7 and h < 1.3, S2 20x10,  7 <= DBH < 20, S3 250x20 DBH >= 20

## BGD: S1 radius 3.99 for dbh < 10 and h < 1.3, S2 250x20 DBH >= 10
## KGZ: S1 radius 3.99 for dbh <  8 and h < 1.3, S2 250x20 DBH >=  8
## LBN: S1 radius 3.99 for dbh < 10 and h < 1.3, S2 250x20 DBH >= 10
## PHL: S1 radius 3.99 for dbh < 10 and h < 1.3, S2 20x10, 10 <= DBH < 20, S3 250x20 DBH >= 20

## CRI
## GTM
## HND
## NIC


