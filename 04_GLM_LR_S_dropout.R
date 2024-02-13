n_knots <- 3

model_s_dropout_adj <- data_S_dropout %>% 
  # Lage aantallen voor nu niet mee
  filter(!immune_status == "Vaccinated and previous infection") %>% 
  mutate(`binary S result` =  case_when(
    `S result` == "Not detected" ~ 1,
    `S result` == "Detected" ~ 0),
    Leeftijdsgroep10 = Leeftijdsgroep10 %>% factor(levels = c("40-49", "10-19", "20-29", "30-39", "50-59", "60-69", "70-79", "80+"))
  ) %>% 
  glm(`binary S result` ~ 
        Afspraak_start_datum +
        immune_status + 
        Leeftijdsgroep10 + 
        Geslacht, 
      family = binomial(logit), 
      data = .)  

# DEZE NIEUWE METHODE VAN CIs schatten implementeren.
OR    <- model_s_dropout_adj %>% coef() %>% exp() %>% as.data.frame() %>% rownames_to_column(var = "var") %>% rename("OR" = ".")
ci_OR <- model_s_dropout_adj %>% confint() %>% exp() %>% as.data.frame()

tabel_s_dropout_adj <- bind_cols(OR = OR, ci = ci_OR) %>% as_tibble() %>%
 mutate(label = str_c(format(round(OR,1), nsmall = 1, trim = T),
                " (",format(round(`2.5 %`,1), nsmall = 1, trim = T),"-",
                format(round(`97.5 %`,1), nsmall = 1, trim = T),")"),
        Data = "Alle data")

model_s_dropout_adj_reis <- data_S_dropout %>% 
  # Lage aantallen voor nu niet mee
  filter(!immune_status == "Vaccinated and previous infection") %>% 
  # No travel history or unknown
  filter(!Reishistorie %in% c("Reishistorie")) %>% 
  mutate(`binary S result` =  case_when(
    `S result` == "Not detected" ~ 1,
    `S result` == "Detected" ~ 0),
  ) %>% 
  glm(`binary S result` ~ Afspraak_start_datum + immune_status + 
        Leeftijdsgroep10 + Geslacht, 
      family = binomial(logit), 
      data = .)  


# DEZE NIEUWE METHODE VAN CIs schatten implementeren.
OR_reis    <- model_s_dropout_adj_reis %>% coef() %>% exp() %>% as.data.frame() %>% rownames_to_column(var = "var") %>% rename("OR" = ".")
ci_OR_reis <- model_s_dropout_adj_reis %>% confint() %>% exp() %>% as.data.frame()

tabel_s_dropout_adj_reis <- bind_cols(OR = OR_reis, ci = ci_OR_reis) %>% as_tibble() %>%
  mutate(label = str_c(format(round(OR,1), nsmall = 1, trim = T),
                       " (",format(round(`2.5 %`,1), nsmall = 1, trim = T),"-",
                       format(round(`97.5 %`,1), nsmall = 1, trim = T),")"),
         Data = "reis")

tabel_1 <- bind_rows(tabel_s_dropout_adj, tabel_s_dropout_adj_reis) %>% 
  filter(var %in% c("immune_statusVaccinated", "immune_statusPrevious infection"))


model_s_dropout_adj_vaccin <- data_S_dropout %>% 
  # Lage aantallen voor nu niet mee
  filter(!is.na(immune_status_vacc)) %>% 
  mutate(`binary S result` =  case_when(
    `S result` == "Not detected" ~ 1,
    `S result` == "Detected" ~ 0),
  ) %>% 
  glm(`binary S result` ~ 
        Afspraak_start_datum +
        immune_status_vacc + 
        Leeftijdsgroep10 + 
        Geslacht, 
      family = binomial(logit), 
      data = .)  


tabel_s_dropout_adj_vaccin <- model_s_dropout_adj_vaccin %>% 
  broom.mixed::tidy() %>% 
  mutate(low  = estimate - (1.96 * std.error),
         high = estimate + (1.96 * std.error),
         OR = exp(estimate),
         OR_low = exp(low),
         OR_high = exp(high),
         label = str_c(format(round(OR,1), nsmall = 1, trim = T),
                       " (",format(round(OR_low,1), nsmall = 1, trim = T),"-", 
                       format(round(OR_high,1), nsmall = 1, trim = T),")"),
         label = if_else(is.na(OR), " ", label),
         Data = "Alle data"
  ) %>% 
  filter(term %in% c("immune_status_vaccCOM, full", "immune_status_vaccJANSS, full",
                     "immune_status_vaccMOD, full","immune_status_vaccAZ, full",
                     "immune_status_vaccPrevious infection"))


model_s_dropout_adj_vaccin_reis <- data_S_dropout %>% 
  # Lage aantallen voor nu niet mee
  filter(!is.na(immune_status_vacc)) %>% 
  # No travel history or unknown
  filter(!Reishistorie %in% c("Reishistorie")) %>% 
  mutate(`binary S result` =  case_when(
    `S result` == "Not detected" ~ 1,
    `S result` == "Detected" ~ 0),
  ) %>% 
  glm(`binary S result` ~ 
        Afspraak_start_datum +  
        immune_status_vacc + 
        Leeftijdsgroep10 + 
        Geslacht, 
      family = binomial(logit), 
      data = .)  


tabel_s_dropout_adj_vaccin_reis <- model_s_dropout_adj_vaccin_reis %>% 
  broom.mixed::tidy() %>% 
  mutate(low  = estimate - (1.96 * std.error),
         high = estimate + (1.96 * std.error),
         OR = exp(estimate),
         OR_low = exp(low),
         OR_high = exp(high),
         label = str_c(format(round(OR,1), nsmall = 1, trim = T),
                       " (",format(round(OR_low,1), nsmall = 1, trim = T),"-", 
                       format(round(OR_high,1), nsmall = 1, trim = T),")"),
         label = if_else(is.na(OR), " ", label),
         Data = "Zonder reishistorie"
  ) %>% 
  filter(term %in% c("immune_status_vaccCOM, full", "immune_status_vaccJANSS, full",
                     "immune_status_vaccMOD, full","immune_status_vaccAZ, full",
                     "immune_status_vaccPrevious infection"))



tabel_2 <- bind_rows(tabel_s_dropout_adj_vaccin, tabel_s_dropout_adj_vaccin_reis)
