##SUMMARY STATS FOR CENSUS DATA##

#Load libraries
library(dplyr)
library(gtsummary)

#Read in census data
setwd("/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Census")
census = read.csv('Census_db_15FEB2024.csv')

#Eliminate family level data, keep indiv level data
census_indiv <- census[!grepl('familia', census$redcap_event_name),]

#Histogram of age
hist(census_indiv$Age,breaks = 20, freq = T, xlab="age", ylab = "Count", main = NULL)

#Create categorical variable for age
census_indiv$age_cat <- case_when(census_indiv$Age >=0 & census_indiv$Age < 10 ~ "0-9",
                            census_indiv$Age >=10 & census_indiv$Age < 20 ~ "10-19",
                            census_indiv$Age >=20 & census_indiv$Age < 30 ~ "20-29",
                            census_indiv$Age>=30  & census_indiv$Age < 40 ~ "30-39",
                            census_indiv$Age >=40 & census_indiv$Age < 50 ~ "40-49",
                            census_indiv$Age >=50 & census_indiv$Age < 60 ~ "50-59",
                            census_indiv$Age >=60 & census_indiv$Age < 70 ~ "60-69",
                            census_indiv$Age >=70 & census_indiv$Age < 80 ~ "70-79",
                            census_indiv$Age >=80 & census_indiv$Age < 90 ~ "80-89",
                            census_indiv$Age >=90 & census_indiv$Age <= 100 ~ "90-100")

#Summary tables
#table(census_indiv$age_cat)
census_indiv %>% select(age_cat) %>% tbl_summary(label = age_cat ~"Categoría de edad")

#Categorical variable for >65 or not
census_indiv$age_cat_65 <- case_when(census_indiv$Age >=0 & census_indiv$Age < 65 ~ "<65",
                                     census_indiv$Age >=65  ~ ">=65")

#Summary table
census_indiv %>% select(age_cat_65) %>% tbl_summary(label = age_cat_65 ~"Categoría de edad")

