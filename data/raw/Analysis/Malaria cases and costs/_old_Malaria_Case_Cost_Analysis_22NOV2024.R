##INCIDENT CASES OF MALARIA DATA ANALYSIS##
#Analyzing data from study enrollment visit

#Set working directory
#setwd("/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Baseline visit/Malaria cases and costs")


rm(list = ls())


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

