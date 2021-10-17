## NFI-modules-2021-learnr
## Gael Sola, FAO

## Remove labels when converting MS Access tables to csv
rm_labels <- function(x){
  for(i in 1 : dim(x)[2]) class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
  for(i in 1 : dim(x)[2]) attr(x[[i]],"label") <- NULL
  return(x)
}

## Extract targeted CSV from MS ACCESS database

## !!! For testing only
#.filepath <- path_mdb[23]

extract_csv <- function(.filepath){
  
  ## Checks
  stopifnot("Hmisc"   %in% installed.packages())
  stopifnot("dplyr"   %in% installed.packages())
  stopifnot("stringr" %in% installed.packages())
  stopifnot("purrr"   %in% installed.packages())
  
  ## Get db names and country
  is_data  <- .filepath %>% str_detect(".*(NFA|NFI|NFMA|ILUA)-data(-|_)")
  filename <- .filepath %>% str_remove(pattern = ".*/") %>% str_remove(".mdb")
  
  if (is_data) {
    
    country <- filename %>%
      str_remove(".*(NFA|NFI|NFMA|ILUA)-data(-|_)") %>%
      str_remove("(-|_).*") %>%
      str_replace("KYR", "Kyrgyzstan") %>%
      str_to_title()
    
  } else {
    
    country <- filename %>%
      str_remove(".*(NFA|NFI|NFMA|ILUA)(-|_)") %>%
      str_remove("(-|_).*") %>%
      str_to_title()
    
  } ## END IF country
  
  
  message(paste0("Extracting database: ", filename, " from: ", country))
  
  
  ## Read DB and create dir
  #mdb.get(.filepath, tables = T)
  mydb        <- mdb.get(.filepath)
  new_path    <- paste0("data/NFMA/csv/", country)
  
  dir.create(new_path, showWarnings = F)
  write_file("", file = file.path(new_path, paste0("DB_name_", filename, ".txt")))
  
  
  ## Save tables
  if (is_data) {
    
    # ## --- Save data tables
    # tabs <- c("F1-Tract", "F2-Plot", "F3-Trees", "F5-LUS")
    # 
    # walk(.x = tabs, .f = function(x) {
    #   tt <- mydb[[x]] %>% rm_labels() %>% as_tibble()
    #   write_csv(tt, file = file.path(new_path, paste0(x %>% str_replace("/", "_"), ".csv")))
    # })
    
    ## --- Save species codes
    if (!(country %in% c("Cameroun", "Guatemala", "Kenya", "Kyrgyzstan", "Lebanon", "Philippines"))) {
      
      tt  <- mydb[["C-Species"]] %>% rm_labels() %>% as_tibble() 
      tt %>%
        select(sp_code = Species.ID, sp_name = SpeciesDefaultScientific) %>%
        write_csv(file = file.path(new_path, "species-codes.csv"))
      
    } ## END IF species code
    
  } else {
    
    # ## --- Land use codes
    # if (country %in% c("Lebanon", "Guatemala", "Cameroun", "Philippines")) {
    # 
    #   tt <- mydb[["C-LandUse/StandType"]] %>% rm_labels() %>% as_tibble()
    #   tt %>%
    #     select(id = LandUse.StandType.ID,  lu = LandUse.StandType.text.eng) %>%
    #     write_csv(file = file.path(new_path, "lu-codes.csv"))
    # 
    # }  else {
    # 
    #   tt  <- mydb[["C-Codes"]] %>% rm_labels() %>% as_tibble()
    #   tt %>%
    #     filter(Variable.ID == 80) %>%
    #     select(id = Code.ID,  lu = Code.text.eng) %>%
    #     write_csv(file = file.path(new_path, "lu-codes.csv"))
    # 
    # } ## END IF LU Codes
    
    ## --- Species codes from code lists
    if (country %in% c("Cameroun", "Kenya", "Kyrgyzstan", "Lebanon", "Philippines")) {
      
      tt  <- mydb[["C-Species"]] %>% rm_labels() %>% as_tibble() 
      tt %>%
        { if (country %in% c("Cameroun"))               select(., sp_code = SpeciesCode, sp_name = SpeciesScientificDefault) else . } %>%
        { if (country %in% c("Kenya", "Kyrgyzstan"))    select(., sp_code = ID.SPECIES , sp_name = SpeciesDefaultScientific) else . } %>%
        { if (country %in% c("Lebanon", "Philippines")) select(., sp_code = Species.ID , sp_name = SpeciesDefaultScientific) else . } %>%
        write_csv(file = file.path(new_path, "species-codes.csv"))
      
    } else if (country == "Guatemala") {
      
      tt  <- mydb[["C-TreeSpecies"]] %>% rm_labels() %>% as_tibble() 
      tt %>%
        select(sp_code = ID, sp_name = TreeSpecies) %>%
        write_csv(file = file.path(new_path, "species-codes.csv"))
      
    } ## END IF Species codes from code lists
    
  } ## END IF write tables
  
} ## END extract_csv()
