##MALARIA DATA ANALYSIS##
#Analyzing data from malaria case visits

#Set working directory
#setwd("/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Baseline visit/Malaria cases and costs")

#Load libraries
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(openxlsx)

Sys.setenv(token_protocol = "E9135070AC82DFAAFA54E4B74BD1C127")



####INDIVIDUAL LEVEL####


#Read in dataset with data from baseline survey- individual level
basal <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 209751, guess_type = F)$data



#elimnate rows where the indiv code is NA
basal <- basal %>% filter(!is.na(base_ind_code))

#add labels to sex variable
basal$base_ind_sex <- factor(basal$base_ind_sex, levels = c(1, 2), labels = c("Male", "Female"))



# Report from REDCap
case <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 210608, guess_type = F)$data

#elimnate rows where the indiv code is NA
case <- case %>% filter(!is.na(cc_participant_code))


#Merge case and basal data together
#rename pt_code variables first
case <- case %>%
  rename(pt_code= cc_participant_code) %>%
  mutate(malaria = 1) %>%
  select (pt_code, cc_registration_date, malaria)


case_first <- case %>%
  arrange(pt_code, cc_registration_date) %>%  # earliest first
  group_by(pt_code) %>%
  slice(1) %>%
  ungroup()


basal_case <-
  basal %>%
  select (base_ind_code : base_ind_g6pd_hemo_result) %>%
  rename(pt_code= base_ind_code) %>% 
  left_join(case_first, by = "pt_code", relationship = "one-to-many")




basal_case <- basal_case %>%
  mutate(base_ind_age = as.numeric(as.character(base_ind_age)), 
         base_ind_g6pd_result= as.numeric(as.character(base_ind_g6pd_result)), 
         base_ind_g6pd_hemo_result= as.numeric(as.character(base_ind_g6pd_hemo_result)), 
         malaria_status = if_else(is.na(malaria), "No Malaria", "Malaria"),
         malaria_status = factor(malaria_status, levels = c("No Malaria", "Malaria")))

#plot Hb vs age w/malaria and w/o malaria
#includes both sexes
ggplot(basal_case, aes(
  x = base_ind_age,
  y = base_ind_g6pd_hemo_result,
  color = factor(malaria),
  fill = factor(malaria),
  group = factor(malaria)
)) +
  geom_point(size = 0.8, alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE, size = 1) +
  xlab("Age (years)") +
  ylab("Hemoglobin (g/dL)") +
  labs(color = "Malaria Status", fill = "Malaria Status") +
  scale_color_manual(values = c("blue", "red"), labels = c("No Malaria", "Malaria")) +
  scale_fill_manual(values = c("blue", "red"), labels = c("No Malaria", "Malaria")) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12)
  )





ggplot(basal_case, aes(
  x = base_ind_age,
  y = base_ind_g6pd_result,
  color = factor(malaria),
  fill = factor(malaria),
  group = factor(malaria)
)) +
  geom_point(size = 0.8, alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE, size = 1) +
  xlab("Age (years)") +
  ylab("G6PD IU/g Hb") +
  labs(color = "Malaria Status", fill = "Malaria Status") +
  scale_color_manual(values = c("blue", "red"), labels = c("No Malaria", "Malaria")) +
  scale_fill_manual(values = c("blue", "red"), labels = c("No Malaria", "Malaria")) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12)
  )



basal_case %>% filter(!is.na(cc_registration_date)) %>% 
  group_by(base_ind_sex) %>%
  summarize(
    min = min(base_ind_age, na.rm = TRUE),
    max = max(base_ind_age, na.rm = TRUE),
    mean = mean(base_ind_age, na.rm = TRUE),
    median = median(base_ind_age, na.rm = TRUE),
    sd = sd(base_ind_age, na.rm = TRUE)
  )

basal_case %>% filter(!is.na(cc_registration_date)) %>% 
  ggplot(aes(x = base_ind_age)) +
  geom_bar()+
  scale_x_continuous(n.breaks=20)



