## NFI-modules-2021-learnr
## Gael Sola, FAO


## This script requires Hmisc and linux package mdbtools (apt-get install mdbtools).

library(Hmisc)


##
## Bloc extract tables from Access DB with standard names #####################
##

## Read all mdb files and save tables of interest in folder with country name with txt file for db version
path_mdb <- list.files(path = "data/NFMA/mdb_files", pattern = ".*(NFA|NFI|NFMA|ILUA)", full.names = TRUE, recursive = TRUE)

## Rename all files with spaces
with_space <- str_subset(path_mdb, " ")
test       <- file.rename(with_space, str_replace_all(with_space, " ", "_"))
if (isTRUE(test)) path_mdb <- list.files(path = "data/NFMA/mdb_files", pattern = ".*(NFA|NFI|NFMA|ILUA)", full.names = TRUE, recursive = TRUE)

## !!! For testing only
# path_data <- str_subset(path_mdb, "-data(-|_)")
# path_app  <- setdiff(path_mdb, path_data)

## Run extract_csv() for all files
walk(.x = path_mdb, .f = extract_csv) 


##
## Manually extract tables for DB with non-standard names ######################
##

## --- Congo -------------------------------------------------------------------

## need to add the C-Codes file manually
temp <- read_csv("data/NFMA/mdb_files/africa/C-Codes-Congo.csv")
temp %>% 
  filter(Variable_ID == 80) %>% 
  select(id = Code_ID,  lu = Code_text_eng) %>%
  write_csv(file = file.path("data/NFMA/csv/Congo", "lu-codes.csv"))

write_file("", file = file.path("data/NFMA/csv/Congo", "codes_from_xlsx.txt"))


## --- Costa Rica --------------------------------------------------------------

## Create path, directory and log file
country <- "CostaRica"
out_path <- paste0("data/NFMA/csv/", country)
dir.create(out_path, showWarnings = F)

## Load data 
db_path <- "data/NFMA/mdb_files/latinamerica/FieldGFS.mdb"
filename <- db_path %>% str_remove(pattern = ".*/") %>% str_remove(".mdb")
write_file("", file = file.path(out_path, paste0("DB_name_", filename, ".txt")))

mdb.get(db_path, tables = T)
db_tmp <- mdb.get(db_path)

## Save tables
db_tmp[["Tract"]]   %>% rm_labels() %>% as_tibble() %>% write_csv(file = file.path(out_path, "F1-Tract.csv"))
db_tmp[["Plot"]]    %>% rm_labels() %>% as_tibble() %>% write_csv(file = file.path(out_path, "F2-Plot.csv"))
db_tmp[["Tree"]]    %>% rm_labels() %>% as_tibble() %>% write_csv(file = file.path(out_path, "F3-Trees.csv"))
db_tmp[["SubPlot"]] %>% rm_labels() %>% as_tibble() %>% write_csv(file = file.path(out_path, "F5-LUS.csv"))

## Load code lists
db_path <- "data/NFMA/mdb_files/latinamerica/CodeTablesGFS.mdb"
filename <- db_path %>% str_remove(pattern = ".*/") %>% str_remove(".mdb")
write_file("", file = file.path(out_path, paste0("DB_name_", filename, ".txt")))

mdb.get(db_path, tables = T)
db_tmp <- mdb.get(db_path)

## Save table
db_tmp[["C-LandUse/StandType"]] %>% rm_labels() %>% as_tibble() %>%
  select(id = LandUse.StandType.ID.t,  lu = LandUse.StandType.text.eng) %>%
  write_csv(file = file.path(out_path, "lu-codes.csv"))

db_tmp[["C-TreeSpecies"]] %>% rm_labels() %>% as_tibble() %>%
  select(sp_code = Treecode,  sp_name = TreeSpecie) %>%
  write_csv(file = file.path(out_path, "species-codes.csv"))


## --- Honduras ----------------------------------------------------------------

## Create path, directory and log file
country <- "Honduras"
out_path <- paste0("data/NFMA/csv/", country)
dir.create(out_path, showWarnings = F)

## Load data 
db_path <- "data/NFMA/mdb_files/latinamerica/ENFHonduras.mdb"
filename <- db_path %>% str_remove(pattern = ".*/") %>% str_remove(".mdb")
write_file("", file = file.path(out_path, paste0("DB_name_", filename, ".txt")))

mdb.get(db_path, tables = T)
db_tmp <- mdb.get(db_path)

## Save tables
db_tmp[["unidadmuestreo"]]   %>% rm_labels() %>% as_tibble() %>% write_csv(file = file.path(out_path, "F1-Tract_spanish.csv"))
db_tmp[["parcela"]]    %>% rm_labels() %>% as_tibble() %>% write_csv(file = file.path(out_path, "F2-Plot_spanish.csv"))
db_tmp[["arbol"]]    %>% rm_labels() %>% as_tibble() %>% write_csv(file = file.path(out_path, "F3-Trees_spanish.csv"))
db_tmp[["claseusotierra"]] %>% rm_labels() %>% as_tibble() %>% write_csv(file = file.path(out_path, "F5-LUS_spanish.csv"))
db_tmp[["clasificacionuso"]] %>% rm_labels() %>% as_tibble() %>% 
  select(id = codigouso,  lu = descripcion) %>%
  write_csv(file = file.path(out_path, "lu-codes_spanish.csv"))
db_tmp[["especie"]] %>% rm_labels() %>% as_tibble() %>% 
  select(sp_code = especie,  sp_name = nombrecientifico) %>%
  write_csv(file = file.path(out_path, "species-codes.csv"))

## --- Nicaragua ---------------------------------------------------------------

## Create path, directory and log file
country <- "Nicaragua"
out_path <- paste0("data/NFMA/csv/", country)
dir.create(out_path, showWarnings = F)

## Load data 
db_path <- "data/NFMA/mdb_files/latinamerica/Inf_Central_06032009.mdb"
filename <- db_path %>% str_remove(pattern = ".*/") %>% str_remove(".mdb")
write_file("", file = file.path(out_path, paste0("DB_name_", filename, ".txt")))

mdb.get(db_path, tables = T)
db_tmp <- mdb.get(db_path)

## Save tables
db_tmp[["UNIDADES_MUESTREOS"]]   %>% rm_labels() %>% as_tibble() %>% write_csv(file = file.path(out_path, "F1-Tract_spanish.csv"))
db_tmp[["PARCELAS"]]    %>% rm_labels() %>% as_tibble() %>% write_csv(file = file.path(out_path, "F2-Plot_spanish.csv"))
db_tmp[["ARBOLES"]]    %>% rm_labels() %>% as_tibble() %>% write_csv(file = file.path(out_path, "F3-Trees_spanish.csv"))
db_tmp[["ESQUEMA_PARCELA_CUT"]] %>% rm_labels() %>% as_tibble() %>% write_csv(file = file.path(out_path, "F5-LUS_spanish.csv"))
db_tmp[["CUT_TB"]] %>% rm_labels() %>% as_tibble() %>% 
  select(id = ID.CUT.TB,  lu = DESCRIPCION) %>%
  write_csv(file = file.path(out_path, "lu-codes_spanish.csv"))
db_tmp[["ESPECIES"]] %>% rm_labels() %>% as_tibble() %>% 
  select(sp_code = ID.ESPECIE,  sp_name = NOMBRE.CIENTIFICO) %>%
  write_csv(file = file.path(out_path, "species-codes.csv"))

## Remove temp objects
rm(with_space)
