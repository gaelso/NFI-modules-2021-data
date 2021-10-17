
## Create Mock data sets for NFI tree, plot and species list, plus ancillary wood density 

## 
## Species code #############################################################
##

length(unique(tree09$accepted_name))
length(unique(tree09$accepted_genus))

mock_tmp <- tree09 %>% 
  mutate(accepted_epithet = str_remove(accepted_name, paste0(word(accepted_name), " "))) %>%
  filter(!is.na(accepted_name)) %>%
  select(accepted_name, accepted_genus, accepted_epithet) %>%
  distinct() %>%
  arrange(accepted_genus, accepted_name)


## Create mock genus and species
count_gn <- length(unique(mock_tmp$accepted_genus))
count_sp <- length(unique(mock_tmp$accepted_epithet))
vowels <- c("a", "e", "i", "o", "u")

set.seed(36)
uuid_gn <- tibble(
  letter   = sample(letters, size = count_gn * 8, replace = T), 
  word     = paste0("n", rep(1:8, count_gn)), 
  mock_num = rep(1:count_gn, 8)
  ) %>%
  pivot_wider(names_from = word, values_from = letter, values_fn = list) %>%
  rowwise() %>%
  mutate(
    raw   = paste0(n1, sample(vowels, 1), n2, n3, sample(vowels, 1), n4, sample(vowels, 1), n5, n6, sample(vowels, 1), n7, sample(vowels, 1), n8, collapse = ""),
    cut   = sample(5:10, 1),
    genus = str_to_title(str_sub(raw, end = cut))
  ) %>%
  bind_cols(accepted_genus = unique(mock_tmp$accepted_genus)) %>%
  select(accepted_genus, genus)

length(unique(uuid_gn$genus))
length(unique(uuid_gn$genus)) == length(unique(uuid_gn$accepted_genus))

uuid_ep <- tibble(
  letter   = sample(letters, size = count_sp * 6, replace = T), 
  word     = paste0("n", rep(1:6, count_sp)), 
  mock_num = rep(1:count_sp, 6)
) %>%
  pivot_wider(names_from = word, values_from = letter, values_fn = list) %>%
  rowwise() %>%
  mutate(
    raw     = paste0(n1, sample(vowels, 1), n2, n3, sample(vowels, 1), n4, sample(vowels, 1), n5, n6, collapse = ""),
    cut     = sample(3:6, 1),
    epithet = str_sub(raw, end = cut)
  ) %>% 
  bind_cols(accepted_epithet = unique(mock_tmp$accepted_epithet)) %>%
  select(accepted_epithet, epithet)

ano_species <- mock_tmp %>%
  left_join(uuid_gn, by = "accepted_genus") %>%
  left_join(uuid_ep, by = "accepted_epithet") %>%
  mutate(
    sp_name = if_else(accepted_epithet == accepted_genus, genus, paste0(genus, " ", epithet)),
    sp_id   = paste0(str_sub(genus, end = 5), if_else(accepted_epithet == accepted_genus, "", paste0("-", str_sub(epithet, end = 4))))
  )

nrow(ano_species) == length(unique(ano_species$sp_id))
length(unique(ano_species$accepted_name)) == length(unique(ano_species$sp_id))

mock_species <- ano_species %>%
  select(sp_id, sp_name, genus) %>%
  bind_rows(list(sp_id = "unk", sp_name = "Unknown", genus = "Unknown"))


##
## Code for plot ID #########################################################
##

nrow(newplot10)
length(unique(newplot10$tract_id))
length(unique(newplot10$plot_id))

table(newplot10$lu_harmo_code)

## Create unique identifier
n <- nrow(newplot10)

set.seed(11)
uuid <- tibble(
  letter   = c(sample(letters, size = n * 3, replace = T), sample(1:9, size = n, replace = T)), 
  word     = paste0("n", rep(1:4, n)), 
  mock_num = rep(1:n, 4) 
) %>%
  pivot_wider(names_from = word, values_from = letter, values_fn = list) %>%
  rowwise() %>%
  mutate(mock_id = paste0(n1, n2, n3, n4, collapse = ""))

## Assign to plot
mock_tmp <- newplot10 %>%
  mutate(mock_num = 1:n) %>%
  left_join(uuid, by = "mock_num") %>%
  mutate(mock_id = paste0(str_to_lower(lu_harmo_code), "-", mock_id))

nrow(mock_tmp) == length(unique(mock_tmp$mock_id))

## Anonymisation table
ano_plot <- mock_tmp %>%
  select(iso, tract_id, plot_id, mock_id)
  
## Anonymized plot
mock_plot <- mock_tmp %>%
  select(plot_id = mock_id, gez_name, gez_code = gez_abbrev, lu_factor, lu_code = lu_harmo_code, envir_stress)
mock_plot

message("Mock plot table plot ID unique?")
print(nrow(mock_plot) == length(unique(mock_plot$plot_id)))


##
## Code for tree ID #########################################################
##

nrow(tree09)
nrow(distinct(tree09))

length(unique(tree09$plot_id))


mock_start <- tree09 %>%
  filter(plot_id %in% ano_plot$plot_id) %>%
  arrange(iso, plot_id) %>%
  mutate(mock_no = 1:nrow(.)) %>%
  group_by(plot_id) %>%
  summarize(mock_start = min(mock_no))
  

mock_tmp <- tree09 %>%
  filter(plot_id %in% ano_plot$plot_id) %>%
  arrange(iso, plot_id) %>%
  left_join(mock_start, by = "plot_id") %>%
  left_join(ano_plot, by = c("iso", "tract_id", "plot_id")) %>%
  left_join(ano_species, by = c("accepted_name", "accepted_genus")) %>%
  mutate(
    mock_no      = 1:nrow(.) - mock_start + 1,
    mock_id_tree = paste0(mock_id, "-", if_else(mock_no < 10, paste0("0", mock_no), as.character(mock_no))),
    sp_id        = if_else(is.na(accepted_name), "unk", sp_id)
    )

ano_tree <- mock_tmp %>%
  select(iso, tract_id, plot_id, tree_no, mock_id, mock_id_tree, mock_no)
 
mock_tree <- mock_tmp %>%
  select(
    plot_id = mock_id, tree_id = mock_id_tree, tree_no = mock_no, 
    tree_dbh, tree_pom, tree_height_top, tree_height_bole, 
    tree_dist, tree_azimuth, tree_health, sp_id
    )

message("Mock tree table tree ID unique?")
print(nrow(mock_tree) == length(unique(mock_tree$tree_id)))


##
## Wood density #############################################################
##

wd_species
wd_genus

mock_wdsp <- wd_species %>%
  right_join(ano_species, by = "accepted_name") %>%
  select(sp_name, wd_avg)

mock_wdgn <- wd_genus %>%
  right_join(ano_species %>% select(accepted_genus, genus) %>% distinct(), by = "accepted_genus") %>%
  select(genus, wd_avg2)
  


##
## Write tables #############################################################
##

## Remove tmp objects
rm(mock_start, mock_tmp)

## Save anonymization codes
dir.create("results/anonymization-codes", showWarnings = F)

tmp_list <- ls(pattern = "ano")
walk(tmp_list, function(x){
  write_csv(get(x), file.path("results/anonymization-codes", paste0(x, ".csv")))
})

## Save mock tables
dir.create("results/mock-data", showWarnings = F)

tmp_list <- ls(pattern = "mock")
walk(tmp_list, function(x){
  write_csv(get(x), file.path("results/mock-data", paste0(x, ".csv")))
})


