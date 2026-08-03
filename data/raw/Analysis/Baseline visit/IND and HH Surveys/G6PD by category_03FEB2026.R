#Analyzing data from study enrollment visit

#Set working directory
#setwd("/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Baseline visit/IND and HH Surveys")

#Load libraries
library(dplyr)
library(tidyverse)
library(openxlsx)
library(readxl)
library(stringr)
library(gtsummary)
library(gt)



####INDIVIDUAL LEVEL####


#Read in dataset with data from baseline survey- individual level
basal <- read.csv("FLAME-Inddatareportsf_29MAY2025.csv")

#elimnate rows where the indiv code is NA
basal <- basal %>% filter(!is.na(base_ind_code))

# Ensure all base_ind_code values have 5 digits
basal$base_ind_code <- str_pad(basal$base_ind_code, width = 5, pad = "0")


#create a new g6pd categorical variable
basal$g6pd_category <- cut(
  basal$base_ind_g6pd_result,
  breaks = c(-Inf, 4.0, 6.1, Inf),  # Define the breakpoints
  labels = c("Deficient", "Intermediate", "Normal"),  # Define the labels
  right = FALSE  # Specify if the intervals are closed on the left or right
)

#test gtsummary
basal |> tbl_summary(include=c('g6pd_category'))


#make mini df w just deficients
deficients <- basal %>% 
  filter(g6pd_category == "Deficient") %>%
  select(base_ind_code, base_ind_g6pd_result, g6pd_category)


#make mini df w just intermediates
intermediates <- basal %>% 
  filter(g6pd_category == "Intermediate") %>%
  select(base_ind_code, base_ind_g6pd_result, g6pd_category)


#make mini df w just normals
normals <- basal %>% 
  filter(g6pd_category == "Normal") %>%
  select(base_ind_code, base_ind_g6pd_result, g6pd_category)

#Select a random group of normals
set.seed(123)
select_normals <- normals %>%  slice_sample(n = 169)

write_csv(deficients, '/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Baseline visit/IND and HH Surveys/codigos_deficientes.csv')
write_csv(intermediates, '/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Baseline visit/IND and HH Surveys/codigos_intermedios.csv')
write_csv(normals, '/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Baseline visit/IND and HH Surveys/codigos_normales.csv')
write_csv(select_normals, '/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Baseline visit/IND and HH Surveys/codigos_select_normales.csv')

