##BASELINE SURVEY ANEMIA DATA ANALYSIS##
#Analyzing data from study enrollment visit

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

#Group by pt code and then create a new variable for the malaria case instance
case <- case %>%
        group_by(cc_participant_code) %>%
        mutate(mal_case = row_number())



write_csv(case, "/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Malaria cases and costs/cases_29JAN2026.csv")


# Report from REDCap
costos <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 210610, guess_type = F)$data

#elimnate rows where the indiv code is NA
costos <- costos %>% filter(!is.na(cost_pt_code))

write_csv(costos, "/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Malaria cases and costs/costs_29JAN2026.csv")



