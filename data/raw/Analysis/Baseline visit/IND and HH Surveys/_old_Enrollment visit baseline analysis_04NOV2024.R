##DATA ANALYSIS FOR ASTMH CONFERENCE PRESENTATION##
#Analyzing data from study enrollment visit

#Set working directory
#setwd("/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Baseline visit/HH Level")

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




####INDIVIDUAL LEVEL####


#Read in dataset with data from baseline survey- individual level
basal <- read.csv('FLAME-Inddatareportsf26NOV2024.csv')

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
  breaks = c(-Inf, 7, 9, 10, 11, Inf),  # Define the breakpoints
  labels = c("Life-threatening", "Severe", "Moderate", "Mild", "Normal"),  # Define the labels
  right = FALSE  # Specify if the intervals are closed on the left or right
  )

#basal %>%
 # filter(base_anemia_cat %in% c("G6PD deficiente", "G6PD intermedio")) %>%
 # select(base_ind_g6pd_hemo_result, anemia_cat) %>%
  #arrange(factor(anemia_cat, levels = c("Life-threatening", "Severe", "Moderate", "Mild", "Normal"))) 


#Creating labels for occupation levels
basal$base_ind_occup <- factor(basal$base_ind_occup, levels = c(0,1,2,3,4,5,6,7,8,9,99), labels = c(
"None", "Agriculture","Wood Collector/Extractor", "Hunter", "Fisherman", "Craftsman/worker", "Merchant",
"Housewife", "Student", "Retired/pensioner", "Other"))

#Creating labels for education levels
basal$base_ind_edu_lvl <- factor(basal$base_ind_edu_lvl, levels = c(0,1,2,3,4,5,6,7,8), labels = c(
 "None", "Incomplete Preschool", "Complete Preschool", "Incomplete Primary", "Complete Primary", "Incomplete Secondary",
 "Complete Secondary","Incomplete University/Technical School", "Complete University/Technical School"))




#Read in dataset with data from clinical history
preg <- read.csv('pregnancy data.csv')

#elimnate men from preg dataset and women who aren't pregnant
preg_filter <- preg %>% filter(mhq_sex==2)
preg_filter <- preg_filter %>% filter(mhq_preg==1)





####INDIV ANALYSIS####

##STARTING ANALYSIS##

#G6PD by sex#
ggplot(basal, aes(x = base_ind_sex, y = base_ind_g6pd_result)) + 
  geom_boxplot()+
  labs(x= "Sex", y = "G6PD IU/g Hg")+
  geom_hline(yintercept = 4.1, color="blue", linetype = "dashed", size = 0.6)+
  geom_hline(yintercept = 6.0, color="skyblue", linetype = "dashed", size = 0.6)+
  annotate("text", x = Inf, y = 4.1, label = "4.1", hjust = 1.2) + 
  annotate("text", x = Inf, y = 6.0, label = "6.0", hjust = 1.2) + 
  theme_light()


#categories of G6PD (deficient, intermediate, normal)
table(basal$g6pd_category)

basal %>%
  filter(base_ind_g6pd_class %in% c("G6PD deficiente", "G6PD intermedio")) %>%
  select(base_ind_code,base_ind_g6pd_class, base_ind_sex, base_ind_age, base_ind_g6pd_result) %>%
  arrange(factor(base_ind_g6pd_class, levels = c("G6PD deficiente", "G6PD intermedio"))) %>%
  slice_head(n = 14)

basal %>%
  filter(base_ind_g6pd_class %in% c("G6PD deficiente")) %>%
  select(base_ind_code,base_ind_g6pd_class, base_ind_sex, base_ind_age, base_ind_g6pd_result) %>%
  arrange(factor(base_ind_g6pd_class, levels = c("G6PD deficiente"))) %>%
  slice_head(n = 5)
  

#Hb by sex
ggplot(basal, aes(x = base_ind_sex, y = base_ind_g6pd_hemo_result)) + 
  geom_boxplot()+
  labs(x= "Sex", y = "Hemoglobin")+
  theme_light()


#Hb by age
ggplot(basal, aes(x = age_category, y = base_ind_g6pd_hemo_result, fill = base_ind_sex)) + 
  geom_boxplot()+
  labs(x= "Age", y = "Hemoglobin", fill ="Sex")+
  ylim(5,20)+
  scale_fill_manual(values = c("Male" = "lightgreen", "Female" = "skyblue"))+
  theme_light()

table(basal$anemia_cat)


ind_table <- basal %>% 
  # Select the variables to be included in the table
  select(#base_ind_age, base_ind_sex,base_ind_diag_lstyr
    base_ind_edu_lvl, base_ind_occup ) %>%
  tbl_summary(
    label=list(
     # base_ind_age ~ "Age",
     # base_ind_sex ~ "Sex",
      base_ind_edu_lvl ~ "Education Level",
      base_ind_occup ~ "Occupation"
    #  base_ind_diag_lstyr ~ "Diagnosed w/ malaria in past yr"
    ),
    statistic = all_categorical() ~ "{n} ({p}%)",  # Show count and percentage
    missing = "no"  # Exclude missing values from the summary
  ) 

gt_ind_table <- as_gt(ind_table)
gtsave(gt_ind_table, "ind_summary_table.html")



#G6PD and Hb levels by sex

g6pd_hb_slim <- basal %>%
  select(age_new, base_ind_sex, base_ind_g6pd_result, base_ind_g6pd_hemo_result)

g6pd_hb_slim <- g6pd_hb_slim %>%
  filter(
    !is.na(age_new) & 
    !is.na(base_ind_sex) & 
    !is.na(base_ind_g6pd_result) & 
    !is.na(base_ind_g6pd_hemo_result)
  )

# Normalize G6PD to match the scale of Hemoglobin
g6pd_hb_slim <- g6pd_hb_slim %>%
  mutate(
    g6pd_scaled = base_ind_g6pd_result/ max(base_ind_g6pd_result, na.rm = TRUE) * max(base_ind_g6pd_hemo_result, na.rm = TRUE)
  )

#g6pd_hb_slim_long <- g6pd_hb_slim %>%
  #pivot_longer(cols = c(g6pd_scaled, base_ind_g6pd_hemo_result),  # Specify columns for hemoglobin and g6pd
              # names_to = "variable", 
              # values_to = "value")




ggplot() +
  # Hemoglobin (primary y-axis)
  geom_smooth(
    data = g6pd_hb_slim,
    aes(x = age_new, y = base_ind_g6pd_hemo_result, color = "Hemoglobin"),
    method = "loess",
    se = TRUE,
    size = 1
  ) +
  # G6PD (secondary y-axis, scaled)
  geom_smooth(
    data = g6pd_hb_slim,
    aes(x = age_new, y = g6pd_scaled, color = "G6PD (Scaled)"),
    method = "loess",
    se = TRUE,
    size = 1
  ) +
  # Primary y-axis
  scale_y_continuous(
    name = "Hemoglobin (g/dL)",
    # Secondary y-axis (transformed scale)
    sec.axis = sec_axis(~ . * max(g6pd_hb_slim$base_ind_g6pd_result, na.rm = TRUE) / max(g6pd_hb_slim$base_ind_g6pd_hemo_result, na.rm = TRUE),
                        name = "G6PD (U/gHb)")
  ) +
  # Customize colors for both lines and legend
  scale_color_manual(
    values = c("Hemoglobin" = "blue", "G6PD (Scaled)" = "red"),
    breaks = c("Hemoglobin", "G6PD (Scaled)"),
    labels = c("Hemoglobin", "G6PD")
  ) +
  labs(
    x = "Age (years)",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "red"),
    axis.title.y.left = element_text(color = "blue")
  )




####HOUSEHOLD LEVEL ANALYSIS####


#Read in dataset with data from baseline survey- household level
house <- read.csv('FLAME-Hhdatareportsf26NOV2024.csv')

#elimnate rows where the hh code is NA
house <- house %>% filter(!is.na(base_hh_house_code))

#Creating labels for wall material
house$base_hh_mat_wall <- factor(house$base_hh_mat_wall, levels = c(1,2,3,4,5,99), labels = c(
  "Ladrillo, cemento", "Madera","Adobe", "Paja", "Palmera (hoja)", "Otro"))

#labels for roof material
house$base_hh_mat_roof <- factor(house$base_hh_mat_roof, levels = c(1,2,3,4,5,99), labels = c(
  "Ladrillo, cemento", "Madera","Calamina", "Paja", "Palmera", "Otro"))

#labels for floor material
house$base_hh_mat_floor <- factor(house$base_hh_mat_floor, levels = c(1,2,3,4,99), labels = c(
  "Tierra", "Madera","Cemento", "Mayólica u otro acabado fino", "Otro"))


#labels for water source
house$base_hh_wtr_source <- factor(house$base_hh_wtr_source, levels = c(1,2,3,4,5,99), labels = c(
  "Intubada fuera/dentro de casa", "Pozo", "Pilón uso público", "Lluvia", "Río o quebrada", "Otro"))

#labels for fumigation
house$base_hh_spryd_insect<- factor(house$base_hh_spryd_insect, levels = c(1,2,98), labels = c(
  "Si", "No", "No sabe"))

#labels for if the head of hh slept under a net last night
house$base_hh_net_hohh<- factor(house$base_hh_net_hohh, levels = c(1,2), labels = c(
  "Si", "No"))


#labels for if the net that the head of hh slept under last night was treated w insecticide
house$base_hh_net_tx<- factor(house$base_hh_net_tx, levels = c(1,2, 98), labels = c(
  "Si", "No", "No sabe"))

#hh use of nets w and w/o insecticide in previous night
ggplot(house, aes(x = base_hh_net_noninsect)) + 
  geom_bar(color="black", fill="lightgreen")+
    labs(x="Non-treated mosquito net count")+
    theme_minimal()

ggplot(house, aes(x = base_hh_net_insect)) + 
  geom_bar()+
  labs(x="Treated mosquito net count")+
  ylim(c(0,100))+
  theme_minimal()


house_long <- house %>%
  pivot_longer(cols = c(base_hh_net_noninsect, base_hh_net_insect),
               names_to = "net_type", 
               values_to = "count") %>%
  mutate(net_type = ifelse(net_type == "base_hh_net_noninsect", "Non-treated", "Treated"))


house_long_filt <- house_long %>% 
  filter(count > 0)

# Create the stacked bar plot
ggplot(house_long_filt, aes(x = count, fill = net_type)) +
  geom_bar(position = "stack") +
  labs(x = "Nets used per HH", y = "Frequency of Response", fill = "Net Type") +
  ggtitle("Previous Night use of Non-treated vs Treated Mosquito Nets") +
  theme_minimal()+
  scale_fill_manual(values=c("Non-treated" ="skyblue","Treated"= "darkblue"))


hh_table <- house %>% 
  # Select the variables to be included in the table
  select(base_hh_mat_wall, base_hh_mat_roof, base_hh_mat_floor, base_hh_wtr_source, base_hh_spryd_insect, base_hh_net_hohh, base_hh_net_tx) %>%
  tbl_summary(
    label=list(
      base_hh_mat_wall ~ "Wall material", 
      base_hh_mat_roof ~ "Roof material", 
      base_hh_mat_floor ~ "Floor material", 
      base_hh_wtr_source ~ "Water source", 
      base_hh_spryd_insect ~ "HH sprayed w insecticide in past yr", 
      base_hh_net_hohh ~ "Did HH head sleep under net last night", 
      base_hh_net_tx ~ "Was that net treated w insecticide"
    ),
    statistic = all_categorical() ~ "{n} ({p}%)",  # Show count and percentage
    missing = "no"  # Exclude missing values from the summary
  )

gt_hh_table <- as_gt(hh_table)
gtsave(gt_hh_table, "hh_summary_table.html")




####Travel History####

table(house$base_hh_hohh_trvl, useNA="always")

house %>% filter(base_hh_hohh_trvl == 1) %>% summarize(mean_travel = mean(base_hh_trvl_quant, na.rm = TRUE))

house %>%
  filter(base_hh_hohh_trvl == 1) %>%
  pull(base_hh_trvl_ovrnght) %>%
  table(useNA = "always")

house %>% filter(base_hh_trvl_ovrnght == 1) %>% summarize(mean_stay = mean(base_hh_nights_spent, na.rm = TRUE))

house %>%
  filter(base_hh_trvl_ovrnght == 1) %>%
  pull() %>%
  table(useNA = "always")



# Select reasons for travelling
travel_responses <- house %>%
  select(base_hh_trip_reasn___1:base_hh_trip_reasn___99)

# Summarize the counts of "1" (selected responses) for each option
travel_summary <- travel_responses %>%
  summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE)))

# View the result
travel_summary

# Select reasons for working
work_responses <- house %>%
  select(base_hh_trip_wrk___0:base_hh_trip_wrk___99)

# Summarize the counts of "1" (selected responses) for each option
work_summary <- work_responses %>%
  summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE)))

# View the result
work_summary



# Select reasons for working
sleep_responses <- house %>%
  select( base_hh_sleep_plce___1: base_hh_sleep_plce___99)

# Summarize the counts of "1" (selected responses) for each option
sleep_summary <- sleep_responses %>%
  summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE)))

# View the result
sleep_summary

#net use while traveling
for (x in 1:3) {
  sleep_column <- paste0("base_hh_sleep_plce___", x)
  net_column <- paste0("base_hh_net_plce", x)
  
  # Execute the logic
  result <- house %>%
    filter(.data[[sleep_column]] == 1) %>%  # Use .data[[...]] for dynamic column names
    pull(.data[[net_column]]) %>%
    table(useNA = "always")
  
  # Print the result
  cat("\nResults for:", sleep_column, "and", net_column, "\n")
  print(result)
}


house %>%
  filter(base_hh_sleep_plce___99== 1) %>% #must change number after ___1
  pull(base_hh_net_plce4) %>%  # must change number after plce
  table(useNA = "always")


####Acceptance/refusal####

#Read in dataset with data from baseline survey- individual level
abord <- read.csv('FLAME-Abordajesf26NOV2024.csv')

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
    summarize(total_all = sum(rowSums(across(c(appr_n_accpt, appr_n_rfs)), na.rm = TRUE))) %>%
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


