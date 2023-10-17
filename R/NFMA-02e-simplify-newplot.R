## NFI-modules-2021-learnr
## Gael Sola, FAO



## Make new plots based on lus subplots #####################################

## Keeps only first LU in case of several LU in the same plot
## Can be improved in a future version to keep dominant LU instead

newplot02tmp <- lus02 |>
  left_join(plot02, by = c("iso", "plot_id")) |>
  filter(plot_length != 596, plot_length != 209, plot_width != 24) |>
  filter(plot_length >= 80, plot_width == 20) |>
  mutate(newplot_id = paste0(plot_id, "_", lus_no))

length(unique(newplot02tmp$plot_id))
length(unique(newplot02tmp$newplot_id))

## Add nb of plots per tract Take first LU if more than 1
count_plot <- newplot02tmp |>
  group_by(iso, tract_id) |>
  summarise(count_plot = n())

count_lus <- newplot02tmp |>
  group_by(iso, plot_id) |>
  summarise(
    count_lus = n(),
    min_lus_no = min(lus_no)
  )

## Keep only first lu recorded in case of several LU
newplot02 <- newplot02tmp |>
  left_join(count_plot, by = c("iso", "tract_id")) |>
  left_join(count_lus, by = c("iso", "plot_id")) |>
  filter(!is.na(tract_id), count_lus == 1 | (count_lus > 1 & lus_no == min_lus_no))

## Checks
length(unique(lus02$plot_id))
length(unique(paste0(lus02$plot_id, "_", lus02$lus_no)))

length(unique(newplot02$plot_id)) == length(unique(paste0(newplot02$plot_id, "_", newplot02$lus_no)))

## rm temp objects
rm(check_lus, count_plot, count_lus, newplot02tmp)


