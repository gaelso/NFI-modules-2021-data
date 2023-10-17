## NFI-modules-2021-learnr
## Gael Sola, FAO


tree08 <- tree07 |>
  mutate(
    tree_height_chave  = exp(0.893 - envir_stress + 0.760 * log(tree_dbh) - 0.0340 * (log(tree_dbh))^2),
    tree_height_ci     = 0.243 * tree_height_chave * 1.96,
    tree_height_valid  = if_else(abs(tree_height_chave - tree_height_top) < tree_height_ci, 1, 0),
    tree_height_cor    = if_else(tree_height_valid != 1 | is.na(tree_height_top), tree_height_chave, tree_height_top),
    tree_height_origin = if_else(tree_height_valid != 1 | is.na(tree_height_top), "model", "data"),
    ) |>
  filter(tree_health <= 2, !(iso %in% c('LBN', 'KGZ')))

summary(tree08$envir_stress)
summary(tree08$tree_height_chave)
summary(tree08$tree_height_top)
summary(tree08$tree_height_cor)

# tt <- tree08 |> filter(plot_id == "AGO_1303622")
# ggplot(tt, aes(x = tree_dbh)) +
#   geom_point(aes(y = tree_height_top)) +
#   geom_line(aes(y = tree_height_es), col = "darkred", size = 1.2) +
#   geom_line(aes(y = tree_height_es + tree_height_ci), col = "red") +
#   geom_line(aes(y = tree_height_es - tree_height_ci), col = "red") +
#   xlim(0, 200) + ylim(0, 50)

# tree08 |> 
#   ggplot(aes(x = tree_dbh)) +
#   geom_point(aes(y = tree_height_cor), size = 1) +
#   geom_line(aes(y = tree_height_chave, color = tract_id), size = 0.1) +
#   facet_wrap(~iso) +
#   theme_bw() +
#   theme(legend.position = "none")

# tree08 |> 
#   ggplot(aes(x = tree_dbh)) +
#   geom_point(aes(y = tree_height_top), size = 1) +
#   geom_point(data = tree_valid, aes(y = tree_height_top), size = 1, col = "darkred") +
#   facet_wrap(~iso) +
#   theme_bw()

gr_h <- tree08 |> 
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = tree_height_top), size = 1) +
  geom_point(aes(y = tree_height_cor), size = 1, col = "darkred") +
  facet_wrap(~iso) +
  theme_bw()
print(gr_h)
