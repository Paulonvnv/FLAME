##DATA ANALYSIS OF ROUND 1A FMDA##

#Set working directory
#setwd("/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/fMDA")

#Load libraries
library(dplyr)
library(tidyr)
library(magrittr)
library(tidyverse)
library(ggplot2)
#library(openxlsx)
#library(readxl)
#library(stringr)



#Read in dataset with data from first day of drug administration- individual level
d1 <- read.csv("FLAME-Interventiond1_21OCT2025.csv")

#elimnate rows where the indiv code is NA
d1 <- d1 %>% filter(!is.na(preint_partcode))

#add labels to sex variable
d1$drug_sex <- factor(d1$drug_sex, levels = c(1, 2), labels = c("Male", "Female"))


# Ensure all participant code values have 5 digits
d1$preint_partcode <- str_pad(d1$preint_partcode, width = 5, pad = "0")

# Ensure all participant code values have 5 digits
d1$drug_community_code <- str_pad(d1$drug_community_code, width = 2, pad = "0")

#create a df specifically for those who met eligibility criteria and received drugs 
d1_gotmeds <- d1 %>% filter(drug_eligibility_review_conclus =="SI CUMPLE LOS REQUISITOS DE ELEGIBILIDAD")

#age by community
  #check to make sure no one is younger than 1 yr old first
d1_gotmeds %>%
  group_by(drug_community_code) %>%
  summarise(
    mean_age = mean(drug_age, na.rm = TRUE),
    sd_age = sd(drug_age, na.rm = TRUE),
    n = n()
  )

#sex by community
d1_gotmeds %>%
  count(drug_community_code, drug_sex)

#g6pd baseline by community
d1_gotmeds %>%
  group_by(drug_community_code) %>%
  summarise(
    mean = mean(drug_g6pd_result_cuant, na.rm = TRUE),
    sd = sd(drug_g6pd_result_cuant, na.rm = TRUE),
    )

#day 1 Hb by community
d1_gotmeds %>%
  group_by(drug_community_code) %>%
  summarise(
    mean = mean(drug_hb_value, na.rm = TRUE),
    sd = sd(drug_hb_value, na.rm = TRUE),
  )


#bring in basal dataset
basal <- read.csv("/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Baseline visit/IND and HH Surveys/FLAME-Inddatareportsf_29MAY2025.csv")


#elimnate rows where the indiv code is NA
basal <- basal %>% filter(!is.na(base_ind_code))

# Ensure all base_ind_code values have 5 digits
basal$base_ind_code <- str_pad(basal$base_ind_code, width = 5, pad = "0")

#Creating labels for occupation levels
basal$base_ind_occup <- factor(basal$base_ind_occup, levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,99), labels = c(
  "None", "Agriculture","Wood Collector/Extractor", "Hunter", "Fisherman", "Craftsman/worker", "Merchant",
  "Housewife", "Student", "Retired/pensioner", "Administration", "Bricklayer", "Driver", "Construction",
  "Guard","Independent", "Motocar Driver", "Laborer", "Security Guard", "Other"))


#Creating labels for education levels
basal$base_ind_edu_lvl <- factor(basal$base_ind_edu_lvl, levels = c(0,1,2,3,4,5,6,7,8), labels = c(
  "None", "Incomplete Preschool", "Complete Preschool", "Incomplete Primary", "Complete Primary", "Incomplete Secondary",
  "Complete Secondary","Incomplete University/Technical School", "Complete University/Technical School"))

#select only education and job variables from basal
basal_min <- basal %>% select(base_ind_code, base_ind_edu_lvl, base_ind_occup)

#merge in education and job variables from basal into d1_gotmeds
d1_gotmeds <- d1_gotmeds %>% 
  left_join( basal_min %>%  
             mutate(base_ind_code = as.character(base_ind_code)),
             by = c( "preint_partcode"= "base_ind_code" ))


#education level by community
d1_gotmeds %>%
  count(drug_community_code, base_ind_edu_lvl)


#education level by community
d1_gotmeds %>%
  count(drug_community_code, base_ind_occup)
