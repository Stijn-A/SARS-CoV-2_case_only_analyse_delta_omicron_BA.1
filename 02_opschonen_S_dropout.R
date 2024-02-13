
# Opschonen

# overzicht: https://cov-lineages.org/index.html#global_reports en https://www.who.int/en/activities/tracking-SARS-CoV-2-variants/, 
# nomenclature Pango: https://www.nature.com/articles/s41564-020-0770-5

start_date <- as.Date("2021-11-22")
end_date <- as.Date("2022-01-19")

infecties_teststraten <- data_teststraten_tmp %>% 
  filter(Uitslag == "Positief" & 
           Afspraak_start_datum < start_date &
           !is.na(Pseudoniem)) %>% 
  select(Pseudoniem, Afspraak_start_datum_1e_inf = Afspraak_start_datum)

# Maak tijdelijk teststraten dataset met minder records om koppeling te versnellen
data_teststraten <- data_teststraten_tmp %>% 
  filter(!is.na(Monsternummer) & Afspraak_start_datum >= as.Date("2021-09-01")) %>% 
  select(Afspraak_start_datum,Afspraak_start_week, Monsternummer, 
         Vaccinatie_status_coronit = Vaccinatie_status,
         Leeftijd, Leeftijdsgroep, Geslacht,
         Vaccinatie_merk_coronit = Vaccinatie_merk, Pseudoniem, 
         Vaccin_aantal_teststraten = Vaccinatie_aantal, 
         Vaccinatie_DUFS_EZD, Ziektegegevens_eerste_ziektedag,
         Vaccinatie_datum_laatste, Klachten) %>%  # NS bedenken welke variabelen nodig zijn
  mutate(
    Vaccinatie_status_coronit = case_when(Vaccinatie_status_coronit == "Pas" ~ "Geen (pas gevaccineerd)", 
                                          Vaccinatie_status_coronit == "Ongevaccineerd" ~ "Geen",
                                          TRUE ~ as.character(Vaccinatie_status_coronit)) %>% 
      factor(levels = c("Volledig", "Deels", "Geen (pas gevaccineerd)", "Geen", "Onbekend")),
    
    Vaccinatie_DUFS_EZD_interval_coronit = as.integer(Vaccinatie_DUFS_EZD - Vaccinatie_datum_laatste)
  )

# Maak tijdelijke kleinere osiris dataset
data_osiris <- data_osiris_tmp %>% 
  filter(Land == "Nederland") %>% 
  filter(!is.na(Monsternummer) & MELGGDOntvDt > as.Date("2021-09-01")) %>% 
  select(Herinfectie, Monsternummer, OSIRISNR, Vaccinatie_status_osiris = Vaccinatie_status_ezd_Desc, 
         Vaccinatie_DUFS_EZD_osiris = Vaccinatie_EZD_interval,Vaccinatie_merk_osiris = Vaccinatie_merk, Vaccin_aantal_osiris = Vaccinatie_aantal_dosis,
         Leeftijdsgroep10, Geslacht_osiris = Geslacht,
         NCOVherinfmeldnr, DUFS_1eziektedag,infectiesoort, NCOVdat1eposncov,ZIE1eZiekteDt,
         NCOVVast1eziektedag, MELGGDOntvDt,Afspraak_start_datum_CoronIT,
         Ziekenhuisopname_Indicatie, `Overleden.Oorzaak`, dagen_tussen_infecties, NCOVherinf,
         Reishistorie,EPILand1Desc,EPILand2Desc,EPILand3Desc,EPILand4Desc,EPILand5Desc,
         dagen_tussen_infecties, ZIE1eZiekteDt
         ) %>%  # NS bedenken welke variabelen verder nuttig zijn
  mutate(NCOVherinfmeldnr = as.integer(NCOVherinfmeldnr),
         Vaccinatie_merk_osiris = case_when(
           Vaccinatie_merk_osiris == "Pfizer/BioNTech" ~ "COM",
           Vaccinatie_merk_osiris == "Spikevax (Moderna)" ~ "MOD",
           Vaccinatie_merk_osiris == "Janssen Phamaceutical Companies" ~ "JANSS",
           Vaccinatie_merk_osiris == "Vaxzevria (AstraZeneca)" ~ "AZ",
           Vaccinatie_merk_osiris == "Heteroloog: 1-AstraZeneca, 2-Pfizer" ~ "AZ-COM",
           TRUE ~ "UNK"
         ),
         Vaccin_aantal_osiris = case_when(
             Vaccin_aantal_osiris == "Geen" ~ 0,
             Vaccin_aantal_osiris == "Een" ~ 1,
             Vaccin_aantal_osiris == "Twee" ~ 2,
             Vaccin_aantal_osiris == "Drie" ~ 3,
             Vaccin_aantal_osiris == "Vier" ~ 4,
           ) %>% as.integer()
         
      )

# zuidelijk_afrika <- c("Zuid-Afrika", "Zimbabwe", "Mozambique",
#                       "Lesotho", "Botswana", "Namibië", "Malawi", "Eswatini",
#                       "Lesotho")

data_S_dropout <- data_S_dropout_tmp %>% 
  mutate(
    `S result` = case_when(
      `S-target` == "Detected" ~ "Detected", # Oudere files hadden hier geen hoofdletter.
      `S-target` == "Undetected" ~ "Not detected",
      TRUE ~ `S-target`
    ) %>% factor(levels = c("Detected","Not detected"))
     ) %>% 
  left_join(data_teststraten,
            by = "Monsternummer") %>% 
  # S detection for Omicron not trushtworthy for Synlab, chosen to start from week 47
  # Also removes samples not present in testingstreets
  filter(Afspraak_start_datum %in% seq(start_date, end_date, 1)) %>% 
  left_join(data_osiris,
            by = "Monsternummer") %>% 
  left_join(infecties_teststraten, by = "Pseudoniem") %>% 
  
  #Use as much info to make vacc_status, first osiris if not present teststraten
  mutate(
    vacc_status = case_when(
      Vaccinatie_status_osiris == "Geen"                       ~ "Not",
      Vaccinatie_status_osiris == "Deels"                      ~ "Partly",
      Vaccinatie_status_osiris == "Volledig"                   ~ "Fully",
      Vaccinatie_status_osiris == "Geen (pas gevaccineerd)"    ~ "Recently vaccinated",
      Vaccinatie_status_coronit == "Geen"                      ~ "Not",
      Vaccinatie_status_coronit == "Deels"                     ~ "Partly",
      Vaccinatie_status_coronit == "Volledig"                  ~ "Fully",
      Vaccinatie_status_coronit == "Geen (pas gevaccineerd)"   ~ "Recently vaccinated",
      
      TRUE ~ "Unknown"
    ) %>% factor(levels = c("Not", "Fully", "Partly", 
                            "Recently vaccinated", "Unknown")),
    
    teststraat_interval = as.integer(Afspraak_start_datum - Afspraak_start_datum_1e_inf),
    
    
    infectie_interval = if_else(!is.na(dagen_tussen_infecties), 
                                as.integer(dagen_tussen_infecties), 
                                teststraat_interval),
    
    Vaccin = case_when(Vaccinatie_status_osiris %in% 
                         c("Geen","Deels","Volledig","Geen (pas gevaccineerd)") ~ as.character(Vaccinatie_merk_osiris),
                       Vaccinatie_status_coronit %in% 
                         c("Geen","Deels","Volledig","Geen (pas gevaccineerd)") ~ as.character(Vaccinatie_merk_coronit)),
    
    Vaccin_aantal = case_when(Vaccinatie_status_osiris %in% 
                         c("Geen","Deels","Volledig","Geen (pas gevaccineerd)") ~ Vaccin_aantal_osiris,
                       Vaccinatie_status_coronit %in% 
                         c("Geen","Deels","Volledig","Geen (pas gevaccineerd)") ~ Vaccin_aantal_teststraten),
    
    Vaccin_EZD_interval = case_when(Vaccinatie_status_osiris %in% 
                                c("Geen","Deels","Volledig","Geen (pas gevaccineerd)") ~ Vaccinatie_DUFS_EZD_osiris,
                              Vaccinatie_status_coronit %in% 
                                c("Geen","Deels","Volledig","Geen (pas gevaccineerd)") ~ Vaccinatie_DUFS_EZD_interval_coronit),
  
    
    Leeftijdsgroep10 = Leeftijd %>%
      cut(
        breaks = c(seq(from = 0, to = 80, by = 10), Inf),
        right = FALSE, include.lowest = TRUE,
        # Create labels, e.g. 0-9, 10-19, ..., 80+
        labels = map2_chr( 
          .x = seq(from = 0, to = 80, by = 10),
          .y = seq(from = 9, to = 89, by = 10),
          .f = str_c, sep = "-") %>% 
          str_replace(pattern = "80-89", replacement = "80+")) %>% 
      fct_explicit_na("Niet vermeld"),
    
    Leeftijdsgroep5 = Leeftijd %>%
      cut(
        breaks = c(seq(from = 0, to = 80, by = 5), Inf),
        right = FALSE, include.lowest = TRUE,
        # Create labels, e.g. 0-9, 10-19, ..., 80+
        labels = map2_chr( 
          .x = seq(from = 0, to = 80, by = 5),
          .y = seq(from = 4, to = 84, by = 5),
          .f = str_c, sep = "-") %>% 
          str_replace(pattern = "80-84", replacement = "80+")) %>% 
      fct_explicit_na("Niet vermeld"),

    herinfectie_teststraat = if_else(
      teststraat_interval > (7*8) &  
        Pseudoniem %in% infecties_teststraten$Pseudoniem, 1, 0),
    #herinfectie_teststraat = if_else(Pseudoniem %in% infecties_teststraten$Pseudoniem, T, F),
    
    Previous_infection = if_else(
      Herinfectie %in% c("Ja, bewezen herinfectie") |
        infectiesoort %in% c("herinfectie") |
        herinfectie_teststraat == T,
      "Reinfection",
      "Infection"
    ) %>% factor(levels = c("Infection", "Reinfection")),
    
    
    immune_status = case_when(
      vacc_status == "Fully" & Previous_infection == "Infection" ~ "Vaccinated",
      vacc_status == "Not" & Previous_infection == "Reinfection" ~ "Previous infection",
      vacc_status == "Fully" & Previous_infection == "Reinfection" ~ "Vaccinated and previous infection",
      vacc_status == "Not" & Previous_infection == "Infection" ~ "Naive"
    ) %>% factor(levels = c("Naive", "Vaccinated", "Previous infection", "Vaccinated and previous infection")),
    
    
    immune_status_vacc = case_when(
      immune_status == "Naive"                                               ~ "Naive",
      # Fully vacc.
      Vaccin == "COM" & immune_status == "Vaccinated"         ~ "COM, full",
      Vaccin == "MOD" & immune_status == "Vaccinated"         ~ "MOD, full",
      Vaccin == "JANSS" & immune_status == "Vaccinated"       ~ "JANSS, full",
      Vaccin == "AZ" & immune_status == "Vaccinated"          ~ "AZ, full",
      
      immune_status == "Previous infection" ~ "Previous infection") %>% 
      factor(levels = c("Naive","COM, full","MOD, full", "JANSS, full", "AZ, full","Previous infection" ))
    
    
    ) %>% 
  filter(!Leeftijdsgroep10 == "Niet vermeld") %>% 
  filter(!Geslacht == "Niet vermeld") %>% 
  group_by(Monsternummer) %>% 
    slice(1) %>% 
  ungroup %>% 
  select(!Pseudoniem) %>% 
  # Voor nu gekozen om de paar mensen met 3-4 prikken eruit te halen. (geen effect op schattingen)
  filter(Vaccin_aantal %in% c(0:2) | is.na(Vaccin_aantal)) %>% 
  filter(!is.na(immune_status)) %>% 
  filter(!immune_status == "Vaccinated and previous infection") %>% 
  filter(Leeftijd >= 12)

#rm(data_osiris_tmp, data_teststraten_tmp)
