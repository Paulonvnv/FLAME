#Checking if DBS list matches case list 

#Set working directory
#setwd("/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Baseline visit/Malaria cases and costs")

#Load libraries
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(openxlsx)

Sys.setenv(token_protocol = "E9135070AC82DFAAFA54E4B74BD1C127")


# Report from REDCap
case <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 210608, guess_type = F)$data

#elimnate rows where the indiv code is NA
case <- case %>% filter(!is.na(cc_participant_code))

case$microscopy_date <- str_sub(case$cc_diag_date, 1, 10)
case$microscopy_date <- as.Date (case$microscopy_date)

case <- case %>% select(autonum:cc_dbs_collected_date, microscopy_date)





#Bring in Flor's internal case file
flor <- read_csv('/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Malaria cases and costs/Tabla recolección de casos incidentes Promotor-Captador_01APR2026.csv')

#take just the 5 digit pt code from flor's file
flor$pt_code <- str_sub(flor$Codigo, 1, 5)
flor$microscopy_date <- as.Date(flor$Captador_Microscopia, format = "%d/%m/%Y")




#create stored values for pt codes in each
flor_ids <- flor$pt_code
case_ids <- case$cc_participant_code

#Overlap
overlap <- intersect(flor_ids, case_ids) #252 codes overlap

length(unique(flor$pt_code)) #252 unique codes in Flor's
length(unique(case$cc_participant_code)) #254 unique codes in case 

diff <- setdiff(flor_ids, case_ids) #0 different

#merge files by pt code
case <- case %>% rename (pt_code = cc_participant_code)


flor <- flor %>%
  group_by(pt_code) %>% 
  mutate(new_id = paste0(pt_code, "_", row_number())) %>%
  ungroup()

case <- case %>%
  group_by(pt_code) %>% 
  mutate(new_id = paste0(pt_code, "_", row_number())) %>%
  ungroup()

join <- left_join(case, flor, by= "new_id")




table (join$Papel_filtro, join$cc_paper_sample_collected, useNA =  "always")





