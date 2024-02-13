


tabel_overview <- bind_rows(
  "Immune_status" = data_S_dropout %>% 
    count(immune_status, `S result`) %>%
    group_by(`S result`) %>% 
      mutate(`%` = (n / sum(n) * 100) %>% round(1)) %>% 
    ungroup %>% 
    pivot_wider(names_from = `S result`, values_from = c(n, `%`)) %>% 
    rename(char = immune_status),

  "Age group" = data_S_dropout %>% 
    count(Leeftijdsgroep10, `S result`) %>%
    group_by(`S result`) %>% 
      mutate(`%` = (n / sum(n) * 100) %>% round(1)) %>% 
    ungroup %>% 
    pivot_wider(names_from = `S result`, values_from = c(n, `%`)) %>% 
    rename(char = Leeftijdsgroep10),
  
  "Sex" = data_S_dropout %>% 
    count(Geslacht, `S result`) %>%
    group_by(`S result`) %>% 
    mutate(`%` = (n / sum(n) * 100) %>% round(1)) %>% 
    ungroup %>% 
    pivot_wider(names_from = `S result`, values_from = c(n, `%`)) %>% 
    rename(char = Geslacht),
  
  "Travel_history" = data_S_dropout %>% 
    mutate(Reishistorie = Reishistorie %>% replace_na("Niet vermeld")) %>% 
    count(Reishistorie, `S result`) %>%
    group_by(`S result`) %>% 
    mutate(`%` = (n / sum(n) * 100) %>% round(1)) %>% 
    ungroup %>% 
    pivot_wider(names_from = `S result`, values_from = c(n, `%`)) %>% 
    rename(char = Reishistorie), .id = "group")




tabel_intervallen <- bind_rows(data_S_dropout %>% 
  group_by(`S result`) %>% 
  summarise(
    median_d_since_vacc = Vaccin_EZD_interval %>% median(na.rm = T) %>% round(1),
    IQR = IQR(Vaccin_EZD_interval, na.rm = T),
    q = quantile(Vaccin_EZD_interval)
  ),

data_S_dropout %>% 
  group_by(`S result`) %>% 
  summarise(
    median_d_since_inf = infectie_interval %>% median(na.rm = T) %>% round(1),
    IQR = IQR(infectie_interval, na.rm = T)
  ))

data_S_dropout %>% 
  filter(immune_status == "Vaccinated") %>% 
  group_by(`S result`) %>% 
  summarise(
    Vaccin_EZD_interval = quantile(Vaccin_EZD_interval, c(0.25, 0.5, 0.75),na.rm = T), q = c(0.25, 0.5, 0.75))
    

data_S_dropout %>% 
  filter(immune_status == "Previous infection") %>% 
  group_by(`S result`) %>% 
  summarise(
    infectie_interval = quantile(infectie_interval, c(0.25, 0.5, 0.75),na.rm = T), q = c(0.25, 0.5, 0.75))



ks.test(data_S_dropout %>% filter(`S result` == "Not detected" & !is.na(infectie_interval)) %>% pull(infectie_interval),
        data_S_dropout %>% filter(`S result` == "Detected" & !is.na(infectie_interval)) %>% pull(infectie_interval))

ks.test(data_S_dropout %>% filter(`S result` == "Not detected" & !is.na(Vaccin_EZD_interval)) %>% pull(Vaccin_EZD_interval),
        data_S_dropout %>% filter(`S result` == "Detected" & !is.na(Vaccin_EZD_interval)) %>% pull(Vaccin_EZD_interval))
