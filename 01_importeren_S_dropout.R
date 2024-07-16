
data_teststraten_tmp <- 
  functie_laad_data("Data/Teststraten/Geschoond/", as.Date("2022-01-23"))  
# data van 20-12-2021 gebruikt voor Medrxiv
# data van 23-01-2022 gebruikt voor eurosurv

data_osiris_tmp <- 
  functie_laad_data("Data/OSIRIS/Geschoond/", as.Date("2022-01-23")) 
# data van 20-12-2021 gebruikt voor Medrxiv
# data van 23-01-2022 gebruikt voor eurosurv


#oude data
S_dropout_data_20211220 <- readRDS("Data/S_dropout_data_20211220.rds")

data_S_dropout_org <- 
  "Data/Voor Stijn_20220121_V2.xlsx" %>% 
  read_excel() %>% 
  rename(Monsternummer = `CoronIT sample number`, `S-target` = `S result`) 

# Samples die weg zijn gevallen omdat nieuwe data is aangeleverd.
data_S_dropout_tmp <- data_S_dropout_org %>% 
  bind_rows(
    S_dropout_data_20211220 %>% 
      filter(!Monsternummer %in% data_S_dropout_org$Monsternummer) %>% 
      rename(`S-target` = `S result`)
    )
