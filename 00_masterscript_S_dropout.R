# Tue Jul 27 10:07:43 2021 ------------------------------
# Author: Stijn P. Andeweg (email: stijn.andeweg@rivm.nl)
# Vaccinatie varianten

### initialize 
setwd("/rivm/n/andewegs/Documents/SARS-CoV-2/03_SGTF_Delta_BA.1")

PATH_surv_data <- "/r-schijf/COVID-19/Surveillance/Data/"

## packages
library(tidyverse)
library(rlang)
library(lubridate)
library(readxl)
library(broom.mixed)
library(ISOweek)
library(splines)
library(cowplot)
library(grid)

## options
options(dplyr.summarise.inform = FALSE) #silence de grouping message
options(scipen = 999) # Do not use exponential notations 

## colors
color_OR <- "#ffb612" # Dark yellow

### import
source(file = "Scripts/01_importeren_S_dropout.R") 
### data preparation
source(file = "Scripts/02_opschonen_S_dropout.R") 

### Tables and Figures
## Figure 1, S1, S3
source(file = "Scripts/03_figuur_S_dropout.R") 
## Figure 1
source(file = "Scripts/04_GLM_LR_S_dropout.R") 

# Overview
source(file = "Scripts/05_Overview_table.R") 

ggsave(file = str_c( 'F1_', format(now(), format = "%Y%m%d_%H%M"), ".pdf"), plot = F1,
       width = 20, height = 10)

list(Overview = tabel_overview,Immune_status = tabel_1, Immune_vaccin = tabel_2, intervallen = tabel_intervallen) %>% 
  writexl::write_xlsx(str_c( "GLM_LR_S-detection_", format(now(), format = "%Y%m%d_%H%M"), ".xlsx"))



### Saving
pdf(file = str_c("Eerste_overzicht_Omicron_v_Delta_", format(now(), format = "%Y%m%d_%H%M"), ".pdf"),
    width = 14, height = 7)
figuur_S_dropout %>% print
figuur_S_dropout_vacc %>% print
figuur_OR %>% print()

figuur_S_dropout_reis %>% print()
figuur_S_dropout_vacc_reis %>% print()
figuur_OR_reis %>% print()
dev.off()

