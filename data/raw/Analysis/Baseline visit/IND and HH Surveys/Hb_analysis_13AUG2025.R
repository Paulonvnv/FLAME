##DATA ANALYSIS FOR ASTMH CONFERENCE PRESENTATION##
#Analyzing data from study enrollment visit

#Set working directory
#setwd("/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Baseline visit/IND and HH Surveys")

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
library(readxl)
library(stringr)




####INDIVIDUAL LEVEL####


#Read in dataset with data from baseline survey- individual level
basal <- read.csv("FLAME-Inddatareportsf_29MAY2025.csv")

#elimnate rows where the indiv code is NA
basal <- basal %>% filter(!is.na(base_ind_code))

#add labels to sex variable
basal$base_ind_sex <- factor(basal$base_ind_sex, levels = c(1, 2), labels = c("Male", "Female"))



#convert bday variable character->numeric
basal$base_ind_bday <- as.Date(basal$base_ind_bday, format = "%Y-%m-%d")  

#calculate new age from birthday variable
today <- Sys.Date()
basal$age_new <- as.numeric(difftime(today, basal$base_ind_bday, units = "weeks")) %/% 52



basal$age_category <- cut(
  basal$age_new,
  breaks = c(-Inf, 16, Inf),  # Define the breakpoints
  labels = c("6 mo-15 yrs", "\u2265 16 yrs"),  # Define the labels
  right = FALSE  # Specify if the intervals are closed on the left or right
)


#create a new g6pd categorical variable
basal$g6pd_category <- cut(
  basal$base_ind_g6pd_result,
  breaks = c(-Inf, 4.0, 6.1, Inf),  # Define the breakpoints
  labels = c("Deficient", "Intermediate", "Normal"),  # Define the labels
  right = FALSE  # Specify if the intervals are closed on the left or right
)


#create a new anemia categorical variable
basal$anemia_cat <- 
  #ifelse(basal$base_ind_sex == 'Female',
  cut(
    basal$base_ind_g6pd_hemo_result,
    breaks = c(-Inf, 8, 11, 12, Inf),  # Define the breakpoints
    labels = c( "Severe", "Moderate", "Mild", "Normal"),  # Define the labels
    right = FALSE  # Specify if the intervals are closed on the left or right
  )

#Checks to confirm variable is defined correctly
basal %>%  filter(anemia_cat == "Severe") %>%  summarize(
  min = min(base_ind_g6pd_hemo_result, na.rm = TRUE),
  max = max(base_ind_g6pd_hemo_result, na.rm = TRUE),
  n = n()  )
basal %>%  filter(anemia_cat == "Moderate") %>%  summarize(
  min = min(base_ind_g6pd_hemo_result, na.rm = TRUE),
  max = max(base_ind_g6pd_hemo_result, na.rm = TRUE),
  n = n()  )
basal %>%  filter(anemia_cat == "Mild") %>%  summarize(
  min = min(base_ind_g6pd_hemo_result, na.rm = TRUE),
  max = max(base_ind_g6pd_hemo_result, na.rm = TRUE),
  n = n()  )


table(basal$anemia_cat, useNA= "always")

severos <- basal %>% filter( anemia_cat =="Severe")
severos <- severos %>% select( base_ind_age, base_ind_age_2, base_ind_sex, base_ind_g6pd_hemo_result)

library(gtsummary)

severos %>%
  select(base_ind_age, base_ind_age_2, base_ind_sex, base_ind_g6pd_hemo_result) %>%
  tbl_summary(
    by = NULL,  # no grouping
    missing = "no" # omit missing counts if not needed
  )
