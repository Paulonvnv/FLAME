##BASELINE SURVEY ANEMIA DATA ANALYSIS##
#Analyzing data from study enrollment visit

#Set working directory
#setwd("/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Baseline visit/ASTMH stats")

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



#Read in dataset with data from baseline survey- individual level
basal <- read.csv('FLAME-Astmhinddatareportsf_IND_22NOV2024.csv')

#elimnate rows where the indiv code is NA
basal <- basal %>% filter(!is.na(base_ind_code))

#add labels to sex variable
basal$base_ind_sex <- factor(basal$base_ind_sex, levels = c(1, 2), labels = c("Male", "Female"))



#convert bday variable character->numeric
basal$base_ind_bday <- as.Date(basal$base_ind_bday, format = "%Y-%m-%d")  

#calculate new age from birthday variable
today <- Sys.Date()
basal$age_new <- as.numeric(difftime(today, basal$base_ind_bday, units = "weeks")) %/% 52


#create new age category variable
basal$age_category <- cut(
  basal$age_new,
  breaks = c(-Inf, 16, Inf),  # Define the breakpoints
  labels = c("6 mo-15 yrs", "\u2265 16 yrs"),  # Define the labels
  right = FALSE  # Specify if the intervals are closed on the left or right
)



#create a new anemia categorical variable
basal$anemia_cat <- 
  #ifelse(basal$base_ind_sex == 'Female',
  cut(
    basal$base_ind_g6pd_hemo_result,
    breaks = c(-Inf, 8, 11, 12, Inf),  # Define the breakpoints
    labels = c("Severe", "Moderate", "Mild", "Normal"),  # Define the labels
    right = FALSE  # Specify if the intervals are closed on the left or right
  )

table(basal$anemia_cat)

basal %>%
  filter(anemia_cat %in% c("Severe", "Moderate", "Mild")) %>%
  select(base_ind_g6pd_hemo_result, anemia_cat) %>%
  slice_head(n = 20)


#Hb by age
ggplot(basal, aes(x = age_category, y = base_ind_g6pd_hemo_result, fill = base_ind_sex)) + 
  geom_boxplot()+
  labs(x= "Age", y = "Hemoglobin", fill ="Sex")+
  ylim(5,20)+
  scale_fill_manual(values = c("Male" = "lightgreen", "Female" = "skyblue"))+
  theme_light()

#All ages
ggplot(basal, aes(x=age_new, y=base_ind_g6pd_hemo_result)) +  
  #geom_point() +
  geom_smooth(method = "loess", se=TRUE, 
              size=0.8, color="red", 
              linetype = "solid", 
              fill="red")+ 
  xlab("Age (years)") +  
  ylab("Hemoglobin (g/dL)")+
  ylim(0,20)+
  theme_minimal()





#Read in dataset with case data
case <- read.csv('FLAME-Casosdemalaria22NOV2024.csv')

#elimnate rows where the indiv code is NA
case <- case %>% filter(!is.na(cc_participant_code))


#Read in dataset with case data
cost <- read.csv('FLAME-Costosdemalariasf22NOV2024.csv')

#elimnate rows where the indiv code is NA
cost <- cost %>% filter(!is.na(cost_pt_code))


#Merge case and cost data together
#rename pt_code variables first
case <- case %>%
  rename(pt_code= cc_participant_code)

cost <- cost %>%
  rename(pt_code= cost_pt_code)
#merge
case_cost <- case %>%
  left_join(cost, by = "pt_code")



#rename pt_code variables first
basal <- basal %>%
  rename(pt_code= base_ind_code)

#merge in data from baseline survey as well 
case_cost_basal <- basal %>%
  left_join(case_cost, by = "pt_code")

case_cost_basal <- case_cost_basal %>%
  mutate(malaria_status = ifelse(is.na(cc_registration_date), 0, 1))


case_cost_basal$malaria_status <- factor(case_cost_basal$malaria_status, levels = c(0, 1), labels = c("No Malaria", "Malaria"))


ggplot(case_cost_basal, aes(
  x = age_new, 
  y = base_ind_g6pd_hemo_result, 
  color = malaria_status, 
  fill = malaria_status, 
  group = malaria_status)) +
  geom_smooth(method = "loess", se = TRUE, size = 0.8) +
  xlab("Age (years)") +
  ylab("Hemoglobin (g/dL)") +
  ylim(0, 20) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), labels = c("No Malaria", "Malaria")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(color = "Malaria Status", fill = "Malaria Status")


