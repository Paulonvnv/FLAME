##BASELINE SURVEY ANEMIA DATA ANALYSIS##
#Analyzing data from study enrollment visit

#Set working directory
#setwd("/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Baseline visit/Malaria cases and costs")

#Load libraries
library(dplyr)
library(tidyr)
#library(magrittr)
library(tidyverse)
library(ggplot2)
#library(rmarkdown)
#library(gtsummary)
#library(gt)
library(openxlsx)



#Read in dataset with data from baseline survey- individual level
basal <- read.csv('/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Baseline visit/IND and HH Surveys/FLAME-Inddatareportsf_26MAR2025.csv')

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


table(basal$anemia_cat)


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
case <- read.csv('FLAME-CasosdemalariaSF_20MAY2025.csv')

#elimnate rows where the indiv code is NA
case <- case %>% filter(!is.na(cc_participant_code))

#Group by pt code and then create a new variable for the malaria case instance
case <- case %>%
        group_by(cc_participant_code) %>%
        mutate(mal_case = row_number())


#Read in dataset with cost data
cost <- read.csv('FLAME-Costosdemalariasf_20MAY2025.csv')

#elimnate rows where the indiv code is NA
cost <- cost %>% filter(!is.na(cost_pt_code))

#Group by pt code and then create a new variable for the malaria case instance
cost <- cost %>%
  group_by(cost_pt_code) %>%
  mutate(mal_case = row_number())


#Merge case and cost data together
#rename pt_code variables first
case <- case %>%
  rename(pt_code= cc_participant_code)

cost <- cost %>%
  rename(pt_code= cost_pt_code)
#merge
case_cost <- 
  left_join(case, cost, by = c("pt_code" , "mal_case"))



#rename pt_code variables first
basal <- basal %>%
  rename(pt_code= base_ind_code)

#merge in data from baseline survey as well 
case_cost_basal <- left_join(case_cost, basal, by = "pt_code")

#create new malaria status variable
case_cost_basal <- case_cost_basal %>%
  mutate(malaria_status = ifelse(is.na(cc_registration_date), 0, 1))

#label malaria status variable
case_cost_basal$malaria_status <- factor(case_cost_basal$malaria_status, levels = c(0, 1), labels = c("No Malaria", "Malaria"))


#plot Hb vs age w/malaria and w/o malaria
#includes both sexes
ggplot(case_cost_basal, aes(
  x = age_new, 
  y = base_ind_g6pd_hemo_result, 
  color = malaria_status, 
  fill = malaria_status, 
  group = malaria_status)) +
  geom_point(size = 0.8, alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE, size = 1) +
  xlab("Age (years)") +
  ylab("Hemoglobin (g/dL)") +
  labs(color = "Malaria Status", fill = "Malaria Status")+
  ylim(0, 20) +
  scale_color_manual(values = c("blue", "red"), labels = c("No Malaria", "Malaria")) +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal() +
  theme(axis.title = element_text(size=14),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size=12), 
        legend.text = element_text(size=11),
        legend.title = element_text(size=12))


#plot Hb vs age w/malaria and w/o malaria
#JUST MALES
case_cost_basal %>%
  filter(base_ind_sex == "Male", 
         !is.na(age_new),        # Remove missing ages
         !is.na(base_ind_g6pd_hemo_result)) %>%
ggplot(aes(
  x = age_new, 
  y = base_ind_g6pd_hemo_result, 
  color = malaria_status, 
  fill = malaria_status, 
  group = malaria_status)) +
  geom_point(size = 0.8, alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE, size = 1) +
  xlab("Age (years)") +
  ylab("Hemoglobin (g/dL)") +
  ylim(0, 20) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), labels = c("No Malaria", "Malaria")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title= "Hemoglobin vs Age (Male Only)", color = "Malaria Status", fill = "Malaria Status")

#plot Hb vs age w/malaria and w/o malaria
#JUST FEMALES
case_cost_basal %>%
  filter(base_ind_sex == "Female", 
         !is.na(age_new),        # Remove missing ages
         !is.na(base_ind_g6pd_hemo_result)) %>%
  ggplot(aes(
    x = age_new, 
    y = base_ind_g6pd_hemo_result, 
    color = malaria_status, 
    fill = malaria_status, 
    group = malaria_status)) +
  geom_point(size = 0.8, alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE, size = 1) +
  xlab("Age (years)") +
  ylab("Hemoglobin (g/dL)") +
  ylim(0, 20) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), labels = c("No Malaria", "Malaria")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title= "Hemoglobin vs Age (Female Only)", color = "Malaria Status", fill = "Malaria Status")



#plot Hb vs age w/malaria and w/o malaria
#EVERYONE, BY SEX
case_cost_basal %>%
  filter(!is.na(age_new),        # Remove missing ages
         !is.na(base_ind_g6pd_hemo_result)) %>%
   mutate(
    base_ind_sex = factor(base_ind_sex, levels = c("Male", "Female")),
    malaria_status = factor(malaria_status, levels = c("No Malaria", "Malaria")),
    interaction_group = interaction(base_ind_sex, malaria_status, lex.order = TRUE) ) %>%
  ggplot(aes(
    x = age_new, 
    y = base_ind_g6pd_hemo_result, 
    color = interaction_group, 
    fill = interaction_group, 
    group = interaction_group)) +
  geom_point(size = 0.8, alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE, size = 1) +
  xlab("Age (years)") +
  ylab("Hemoglobin (g/dL)") +
  ylim(0, 20) +
  theme_minimal() +
  scale_color_manual(
    values = c("lightblue", "blue", "lightgreen", "darkgreen"), 
    labels = c("Male No Malaria", "Male Malaria", "Female No Malaria", "Female Malaria")) +
  scale_fill_manual(
    values = c("lightblue", "blue", "lightgreen", "darkgreen"), 
    labels = c("Male No Malaria", "Male Malaria", "Female No Malaria", "Female Malaria")) +
  labs(title= "Hemoglobin vs Age by Sex and Malaria Status", color = "Malaria Status", fill = "Malaria Status")




#plot G6PD vs age w/malaria and w/o malaria
#includes both sexes
ggplot(case_cost_basal, aes(
  x = age_new, 
  y = base_ind_g6pd_result, 
  color = malaria_status, 
  fill = malaria_status, 
  group = malaria_status)) +
  geom_point(size = 0.8, alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE, size = 1) +
  xlab("Age (years)") +
  ylab("G6PD IU/g Hb") +
  #ylim(0, 20) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), labels = c("No Malaria", "Malaria")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(color = "Malaria Status", fill = "Malaria Status")


#plot G6PD vs age w/malaria and w/o malaria
#JUST MALES
case_cost_basal %>%
  filter(base_ind_sex == "Male", 
         !is.na(age_new),        # Remove missing ages
         !is.na(base_ind_g6pd_result)) %>%
  ggplot(aes(
    x = age_new, 
    y = base_ind_g6pd_result, 
    color = malaria_status, 
    fill = malaria_status, 
    group = malaria_status)) +
  geom_point(size = 0.8, alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE, size = 1) +
  xlab("Age (years)") +
  ylab("G6PD IU/g Hb") +
  ylim(0, 20) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), labels = c("No Malaria", "Malaria")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title= "G6PD vs Age (Male Only)", color = "Malaria Status", fill = "Malaria Status")

#plot G6PD vs age w/malaria and w/o malaria
#JUST FEMALES
case_cost_basal %>%
  filter(base_ind_sex == "Female", 
         !is.na(age_new),        # Remove missing ages
         !is.na(base_ind_g6pd_result)) %>%
  ggplot(aes(
    x = age_new, 
    y = base_ind_g6pd_result, 
    color = malaria_status, 
    fill = malaria_status, 
    group = malaria_status)) +
  geom_point(size = 0.8, alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE, size = 1) +
  xlab("Age (years)") +
  ylab("G6PD IU/g Hb") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), labels = c("No Malaria", "Malaria")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title= "G6PD vs Age (Female Only)", color = "Malaria Status", fill = "Malaria Status")





#plot G6PD vs age w/malaria and w/o malaria
#EVERYONE, BY SEX
case_cost_basal %>%
  filter(!is.na(age_new),        # Remove missing ages
         !is.na(base_ind_g6pd_result)) %>%
  mutate(
    base_ind_sex = factor(base_ind_sex, levels = c("Male", "Female")),
    malaria_status = factor(malaria_status, levels = c("No Malaria", "Malaria")),
    interaction_group = interaction(base_ind_sex, malaria_status, lex.order = TRUE) ) %>%
  ggplot(aes(
    x = age_new, 
    y = base_ind_g6pd_result, 
    color = interaction_group, 
    fill = interaction_group, 
    group = interaction_group)) +
  geom_point(size = 0.8, alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE, size = 1) +
  xlab("Age (years)") +
  ylab("G6PD IU/g Hb") +
  ylim(0, 20) +
  theme_minimal() +
  scale_color_manual(
    values = c("lightblue", "blue", "lightgreen", "darkgreen"), 
    labels = c("Male No Malaria", "Male Malaria", "Female No Malaria", "Female Malaria")) +
  scale_fill_manual(
    values = c("lightblue", "blue", "lightgreen", "darkgreen"), 
    labels = c("Male No Malaria", "Male Malaria", "Female No Malaria", "Female Malaria")) +
  labs(title= "G6PD vs Age by Sex and Malaria Status", color = "Malaria Status", fill = "Malaria Status")




#### Descriptive statistics of malaria cases####

case_cost_basal %>% filter(!is.na(cc_registration_date)) %>% 
  group_by(base_ind_sex) %>%
  summarize(
    min = min(base_ind_age, na.rm = TRUE),
    max = max(base_ind_age, na.rm = TRUE),
    mean = mean(base_ind_age, na.rm = TRUE),
    median = median(base_ind_age, na.rm = TRUE),
    sd = sd(base_ind_age, na.rm = TRUE)
  )

case_cost_basal %>% filter(!is.na(cc_registration_date)) %>% 
  ggplot(aes(x = base_ind_age)) +
  geom_bar()+
  scale_x_continuous(n.breaks=20)



#Addition of g6pd status variable to case_cost_basal dataset
#See the G6PD status of those who have malaria


#create a new g6pd categorical variable
case_cost_basal$g6pd_category <- cut(
  case_cost_basal$base_ind_g6pd_result,
  breaks = c(-Inf, 4.0, 6.1, Inf),  # Define the breakpoints
  labels = c("Deficient", "Intermediate", "Normal"),  # Define the labels
  right = FALSE  # Specify if the intervals are closed on the left or right
)

case_cost_basal %>%
  filter(!is.na(g6pd_category)) %>%
  count(g6pd_category, base_ind_sex, malaria_status)

case_cost_basal %>%
  filter(base_ind_sex== "Male", malaria_status == "Malaria") %>%
  count(base_ind_sex, age_new, malaria_status, base_ind_g6pd_result)



##How many malaria cases have there been, when were they, and which are still missing the cost form associated
#with that case
temp_df<- case_cost_basal %>%
 filter(!is.na(cc_registration_date)) %>%
 count(pt_code, cc_registration_date, cost_date)
 