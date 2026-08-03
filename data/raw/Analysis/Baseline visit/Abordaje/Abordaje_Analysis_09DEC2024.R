##DATA ANALYSIS FOR ABORDAJE VIVIENDA##

#Set working directory
#setwd("/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Baseline visit/Abordaje")

#Load libraries
library(dplyr)
library(tidyr)
library(magrittr)
library(tidyverse)
library(ggplot2)
library(rmarkdown)
library(gtsummary)
library(gt)
library(openxlsx)



#Read in dataset with data from baseline survey- abordaje vivienda
abord <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 209923, guess_type = F)$data


#eliminate rows from Palo Seco, community 22, because questions about accept/refuse participation 
#didn't exist when we intervened there
abord <- abord %>% filter(appr_comm_id != 22)

#Make variables numeric
abord <- abord %>% mutate(across(c(appr_n_accpt, appr_n_rfs),as.numeric))



####Acceptance/refusal####

abord_summary <- abord %>%
  group_by(appr_comm_id) %>%
  summarise(
    total_accpt = sum(appr_n_accpt, na.rm = TRUE),
    total_rfs   = sum(appr_n_rfs, na.rm = TRUE),
    total_all   = total_accpt + total_rfs,
    house_accpt = n_distinct(appr_hh_code[appr_n_accpt > 0]),
    total_hh    = n_distinct(appr_hh_code),
    pct_accepted = round(100 * total_accpt / total_all, 1),
    pct_refused  = round(100 * total_rfs / total_all, 1),
    .groups = "drop"
  ) %>%
  arrange(appr_comm_id) %>%
  rename(Community = appr_comm_id)








# Create an empty list to store results
result_list <- list()

# Loop through each unique community number
unique_communities <- unique(abord$appr_comm_id)

for (community in unique_communities) {
  cat("Community:", community, "\n")
  
  # Filter data for the current community
  total_accpt <- abord %>%
    filter(appr_comm_id == community) %>%
    summarize(total_accpt = sum(appr_n_accpt, na.rm = TRUE)) %>%
    pull(total_accpt)
  
  total_rfs <- abord %>%
    filter(appr_comm_id == community) %>%
    summarize(total_rfs = sum(appr_n_rfs, na.rm = TRUE)) %>%
    pull(total_rfs)
  
  total_all <- abord %>%
    filter(appr_comm_id == community) %>%
    summarize(total_all = sum(appr_n_accpt + appr_n_rfs, na.rm = TRUE)) %>%
    pull(total_all)
  
  house_accpt <- abord %>%
    filter(appr_comm_id == community) %>%
    filter(appr_n_accpt > 0) %>%
    summarize(house_accpt = n()) %>%
    pull(house_accpt)
  
  total_hh <- abord %>%
    filter(appr_comm_id == community) %>%
    summarize(total_hh = n_distinct(appr_hh_code)) %>%
    pull(total_hh)
  
  cat("Total Accepted: ", total_accpt, "\n")
  cat("Total Refused: ", total_rfs, "\n")
  cat("Total population:", total_all, "\n")
  cat("Total HH Accepted:", house_accpt, "\n")
  cat("Total HH", total_hh, "\n")
  cat("\n-------------------\n")
}


#Filter abord dataset for only communities that were enrolled after the addition of 
#reasons for not enrolling questions 
#Communities, 1, 2, 9, 13, 15 are excluded 

abord_filter <- abord %>% filter(!appr_comm_id %in% c(13))

#labels for refusal reasons
abord_filter$appr_rfs_rsn_1 <- factor(abord_filter$appr_rfs_rsn_1, levels = c(1,2,3,4,5,6,7,8,9,10,11,99), labels = c(
  "No tiene DNI a mano","Esta ocupado","Ambos padres biologicos no estan presentes",
  "Vive en otra casa y le registraremos en la otra casa","No tiene interes de participar en el estudio","Religion",
  "Tiene menos de 6 meses de edad", "Viaje", "Trabajo", "Padre sin DNI", 
  "Padre no desea que participe(n) sus(s) hijo(s)", "Otra"))
abord_filter$appr_rfs_rsn_2 <- factor(abord_filter$appr_rfs_rsn_2, levels = c(1,2,3,4,5,6,7,8,9,10,11,99), labels = c(
  "No tiene DNI a mano","Esta ocupado","Ambos padres biologicos no estan presentes",
  "Vive en otra casa y le registraremos en la otra casa","No tiene interes de participar en el estudio","Religion",
  "Tiene menos de 6 meses de edad", "Viaje", "Trabajo", "Padre sin DNI", 
  "Padre no desea que participe(n) sus(s) hijo(s)", "Otra"))
abord_filter$appr_rfs_rsn_3 <- factor(abord_filter$appr_rfs_rsn_3, levels = c(1,2,3,4,5,6,7,8,9,10,11,99), labels = c(
  "No tiene DNI a mano","Esta ocupado","Ambos padres biologicos no estan presentes",
  "Vive en otra casa y le registraremos en la otra casa","No tiene interes de participar en el estudio","Religion",
  "Tiene menos de 6 meses de edad", "Viaje", "Trabajo", "Padre sin DNI", 
  "Padre no desea que participe(n) sus(s) hijo(s)", "Otra"))
abord_filter$appr_rfs_rsn_4 <- factor(abord_filter$appr_rfs_rsn_4, levels = c(1,2,3,4,5,6,7,8,9,10,11,99), labels = c(
  "No tiene DNI a mano","Esta ocupado","Ambos padres biologicos no estan presentes",
  "Vive en otra casa y le registraremos en la otra casa","No tiene interes de participar en el estudio","Religion",
  "Tiene menos de 6 meses de edad", "Viaje", "Trabajo", "Padre sin DNI", 
  "Padre no desea que participe(n) sus(s) hijo(s)", "Otra"))
abord_filter$appr_rfs_rsn_5 <- factor(abord_filter$appr_rfs_rsn_5, levels = c(1,2,3,4,5,6,7,8,9,10,11,99), labels = c(
  "No tiene DNI a mano","Esta ocupado","Ambos padres biologicos no estan presentes",
  "Vive en otra casa y le registraremos en la otra casa","No tiene interes de participar en el estudio","Religion",
  "Tiene menos de 6 meses de edad", "Viaje", "Trabajo", "Padre sin DNI", 
  "Padre no desea que participe(n) sus(s) hijo(s)", "Otra"))
abord_filter$appr_rfs_rsn_6 <- factor(abord_filter$appr_rfs_rsn_6, levels = c(1,2,3,4,5,6,7,8,9,10,11,99), labels = c(
  "No tiene DNI a mano","Esta ocupado","Ambos padres biologicos no estan presentes",
  "Vive en otra casa y le registraremos en la otra casa","No tiene interes de participar en el estudio","Religion",
  "Tiene menos de 6 meses de edad", "Viaje", "Trabajo", "Padre sin DNI", 
  "Padre no desea que participe(n) sus(s) hijo(s)", "Otra"))
abord_filter$appr_rfs_rsn_7 <- factor(abord_filter$appr_rfs_rsn_7, levels = c(1,2,3,4,5,6,7,8,9,10,11,99), labels = c(
  "No tiene DNI a mano","Esta ocupado","Ambos padres biologicos no estan presentes",
  "Vive en otra casa y le registraremos en la otra casa","No tiene interes de participar en el estudio","Religion",
  "Tiene menos de 6 meses de edad", "Viaje", "Trabajo", "Padre sin DNI", 
  "Padre no desea que participe(n) sus(s) hijo(s)", "Otra"))
abord_filter$appr_rfs_rsn_8 <- factor(abord_filter$appr_rfs_rsn_8, levels = c(1,2,3,4,5,6,7,8,9,10,11,99), labels = c(
  "No tiene DNI a mano","Esta ocupado","Ambos padres biologicos no estan presentes",
  "Vive en otra casa y le registraremos en la otra casa","No tiene interes de participar en el estudio","Religion",
  "Tiene menos de 6 meses de edad", "Viaje", "Trabajo", "Padre sin DNI", 
  "Padre no desea que participe(n) sus(s) hijo(s)", "Otra"))
abord_filter$appr_rfs_rsn_9 <- factor(abord_filter$appr_rfs_rsn_9, levels = c(1,2,3,4,5,6,7,8,9,10,11,99), labels = c(
  "No tiene DNI a mano","Esta ocupado","Ambos padres biologicos no estan presentes",
  "Vive en otra casa y le registraremos en la otra casa","No tiene interes de participar en el estudio","Religion",
  "Tiene menos de 6 meses de edad", "Viaje", "Trabajo", "Padre sin DNI", 
  "Padre no desea que participe(n) sus(s) hijo(s)", "Otra"))
abord_filter$appr_rfs_rsn_10 <- factor(abord_filter$appr_rfs_rsn_10, levels = c(1,2,3,4,5,6,7,8,9,10,11,99), labels = c(
  "No tiene DNI a mano","Esta ocupado","Ambos padres biologicos no estan presentes",
  "Vive en otra casa y le registraremos en la otra casa","No tiene interes de participar en el estudio","Religion",
  "Tiene menos de 6 meses de edad", "Viaje", "Trabajo", "Padre sin DNI", 
  "Padre no desea que participe(n) sus(s) hijo(s)", "Otra"))



#Summarize reasons for refusal
# First, pivot your data into a longer format
#refusal_long <- abord_filter %>%
 # pivot_longer(
  #  cols = matches("^appr_rfs_rsn"),
    #cols = matches("^appr_rfs_rsn_\\d+$"), 
 #   names_to = "reason_variable",
 #   values_to = "answer_option"
#  )

refusal_long <- abord_filter %>%
  mutate(across(matches("^appr_rfs_rsn"), as.character)) %>%
  pivot_longer(
    cols = matches("^appr_rfs_rsn_\\d+$"), 
    names_to = "reason_variable",
    values_to = "answer_option"
  )

refusal_long <- refusal_long %>% filter(!is.na(answer_option) & answer_option != "") 

refusal_summary <- refusal_long %>%
  count(reason_variable, answer_option) %>%
  pivot_wider(
    names_from = answer_option, # Turn answer options into columns
    values_from = n,            # Fill cells with counts
    values_fill = 0             # Fill missing combinations with 0
  )

# View the result
write.xlsx(refusal_summary, file = "refusal_summary_31OCT2025.xlsx", append = FALSE)






#Xue's code
df_indi <- abord_filter %>% 
  mutate(across(matches("^appr_rfs_rsn"), as.character)) %>%
  pivot_longer(cols = c(appr_rfs_rsn_1, appr_rfs_rsn_2, appr_rfs_rsn_3, appr_rfs_rsn_4, appr_rfs_rsn_5, appr_rfs_rsn_6,
                        appr_rfs_rsn_7, appr_rfs_rsn_8, appr_rfs_rsn_9, appr_rfs_rsn_10, appr_rfs_rsn_11))

aggregate(df_indi$autonum, by = list(df_indi$value),length)



#write.xlsx(df_indi, file = "xue_refusal_31OCT2025.xlsx", append = FALSE)

