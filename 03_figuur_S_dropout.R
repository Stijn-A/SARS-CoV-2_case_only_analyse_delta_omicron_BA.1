 source("/rivm/r/COVID-19/Surveillance/Scripts/01_Initialiseren/functies/functie_integer_breaks.R")
# 
# 
tabel_S_dropout <- data_S_dropout %>%
  count(Afspraak_start_datum, `S result`) %>%
  group_by(Afspraak_start_datum) %>%
  mutate(
    total = sum(n),
    `%` = n / sum(n) * 100,
    `%` = if_else(`S result` == "Detected", 100 - `%`, `%`)
  ) %>%
  ungroup %>%
  mutate(max_y =`%` %>% max(na.rm = T) %>% "/"(5) %>% ceiling() %>% "*"(5),
         n_scaled = max_y*n /max(total))

figuur_S_dropout <- tabel_S_dropout %>%
  ggplot(data = ., aes(x = Afspraak_start_datum)) +
  geom_col(aes(y = n_scaled, fill = `S result`, colour = `S result`),
           alpha = .75) +
  geom_line(aes(y = `%`), colour = "black") +
  #scale_x_discrete(breaks = function(x) {x[c(TRUE, FALSE)] <- '';x}) +
  scale_y_continuous(
    limits = c(0,tabel_S_dropout %>% pull(max_y) %>% unique()),
    breaks = functie_integer_breaks(),
    sec.axis = sec_axis(
      trans = function(y_axis) {
        const <- tabel_S_dropout %>%
          transmute(total/max_y) %>% # hier neem ik 100 als max
          max()
        y_axis*const},
      name = "Counts")) +
  scale_fill_brewer(type = "qual",palette = 1) +
  scale_colour_brewer(type = "qual",palette = 1) +
  #ylab("Counts") +
  scale_x_date(
    limits = c(min(tabel_S_dropout$Afspraak_start_datum) - 1, max(tabel_S_dropout$Afspraak_start_datum) + 1),
    expand = expansion(add = -0.5),
    # Maatstreeplabel per week
    breaks = "2 day", date_labels = "%e %b",
    minor_breaks = NULL) +
  labs(
    x = "Sample date",
    y = "% S Not detected") +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.title = element_text(size = 10))



tabel_S_dropout_vacc <- data_S_dropout %>% 
  filter(immune_status %in% c("Naive", "Vaccinated", "Previous infection")) %>% 
  count(Afspraak_start_datum, immune_status)

F1_left <- tabel_S_dropout_vacc %>% 
  ggplot(data = ., aes(x = Afspraak_start_datum)) +
  geom_col(aes(y = n, fill = immune_status, colour = immune_status),
           alpha = .75) +
  scale_fill_brewer(type = "qual",palette = 1, name = "Immune status") +
  scale_colour_brewer(type = "qual",palette = 1, name = "Immune status") +
  theme_minimal() +
  labs(y = "Counts",
       x = "Sample date",
       ) +
  scale_x_date(
    limits = c(min(tabel_S_dropout_vacc$Afspraak_start_datum) - 1, max(tabel_S_dropout_vacc$Afspraak_start_datum) + 1),
    expand = expansion(add = -0.5),
    # Maatstreeplabel per week
    breaks = "2 day", date_labels = "%e %b",
    minor_breaks = NULL)

legend <- get_legend(
  F1_left + theme(legend.key.size = unit(1, 'cm'),
                   text = element_text(size=20),
                  legend.position = "bottom"))

F1_left <- F1_left + theme(
  axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.5),
  text = element_text(size=20),
  legend.position = "none")



tabel_S_dropout_IS <- data_S_dropout %>% 
  filter(immune_status %in% c("Naive", "Vaccinated", "Previous infection")) %>% 
  count(Afspraak_start_datum,`S result`, immune_status) %>% 
  group_by(Afspraak_start_datum,immune_status) %>% 
  mutate(
    `%` = n / sum(n) * 100
  ) %>% 
  ungroup %>% 
  filter(`S result` == "Not detected") %>% 
  select(Afspraak_start_datum, immune_status, `%`) %>% 
  complete(Afspraak_start_datum, nesting(immune_status), fill = list(`%` = 0))

F1_right <- tabel_S_dropout_IS %>% 
  ggplot(data = ., aes(x = Afspraak_start_datum)) +
    geom_line(aes(y = `%`, colour = immune_status),size = 1.2,
             alpha = 1) +
    scale_fill_brewer(type = "qual",palette = 1, name = "Immune status") +
    scale_colour_brewer(type = "qual",palette = 1, name = "Immune status") +
    theme_minimal() +
    theme(
      axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.5),
      text = element_text(size=20),
      legend.position = "none") +
    labs(y = "% S-gene target failure",
         x = "Sample date") +
  scale_x_date(
    limits = c(min(tabel_S_dropout_vacc$Afspraak_start_datum) - 1, max(tabel_S_dropout_vacc$Afspraak_start_datum) + 1),
    expand = expansion(add = -0.5),
    # Maatstreeplabel per week
    breaks = "2 day", date_labels = "%e %b",
    minor_breaks = NULL) +
  scale_y_continuous(
    limits = c(0,100),
    breaks = seq(0,100,5),
    minor_breaks  = NULL) 


F1 <- plot_grid(
  plot_grid(
    F1_left, F1_right, align=c("h"),ncol = 2),
  legend, ncol = 1, rel_heights = c(1,0.05))


