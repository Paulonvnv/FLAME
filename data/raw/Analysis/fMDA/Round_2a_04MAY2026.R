#Round 2a fMDA Analysis

library(dplyr)
library(REDCapR)
#library(lubridate)
#library(tidyverse)
#library(ggplot2)
#library (janitor)
#library(gtsummary)

Sys.setenv(token_protocol = "E9135070AC82DFAAFA54E4B74BD1C127")


# Report from REDCAp - FLAME for the base line forms

df_intervention_day_1_2a <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 263194, guess_type = F)$data

df_intervention_day_2_2a <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 263195, guess_type = F)$data

df_intervention_day_3_2a <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 263196, guess_type = F)$data

df_intervention_day_4_2a <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 263197, guess_type = F)$data

df_intervention_day_5_2a <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 263198, guess_type = F)$data

df_intervention_day_6_2a <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 263199, guess_type = F)$data

df_intervention_day_7_2a <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 263200, guess_type = F)$data



#combine all dias of intervention, filtered where the drug day of intervention question for that day is ==1 
all_days_2a <- df_intervention_day_1_2a %>% 
  filter(redcap_repeat_instrument=="intervencion_2_a_dia_1") %>% 
  filter(int2a_drug_day_of_intervention_1=="1") %>% # (149)
  distinct(int2a_preint_partcode, .keep_all = T)%>%
  left_join(
    df_intervention_day_2_2a %>% 
      filter(int2a_drug_day_of_intervention_2=="1") %>%
      distinct(int2a_preint_partcode_2, .keep_all = T),
    by = c("int2a_preint_partcode"="int2a_preint_partcode_2")) %>% 
  left_join(
    df_intervention_day_3_2a %>% 
      filter(int2a_drug_day_of_intervention_3=="1")%>%
      distinct(int2a_preint_partcode_3, .keep_all = T),
    by = c("int2a_preint_partcode"="int2a_preint_partcode_3")) %>%
  
  left_join(
    df_intervention_day_4_2a %>% 
      filter(int2a_drug_day_of_intervention_4=="1")%>%
      distinct(int2a_preint_partcode_4, .keep_all = T),
    by = c("int2a_preint_partcode"="int2a_preint_partcode_4")) %>%
  left_join(
    df_intervention_day_5_2a %>% 
      filter(int2a_drug_day_of_intervention_5=="1")%>%
      distinct(int2a_preint_partcode_5, .keep_all = T),
    by = c("int2a_preint_partcode"="int2a_preint_partcode_5")) %>%
  left_join(
    df_intervention_day_6_2a %>% 
      filter(int2a_drug_day_of_intervention_6=="1")%>%
      distinct(int2a_preint_partcode_6, .keep_all = T),
    by = c("int2a_preint_partcode"="int2a_preint_partcode_6")) %>%
  left_join(
    df_intervention_day_7_2a %>% 
      filter(int2a_drug_day_of_intervention_7=="1")%>%
      distinct(int2a_preint_partcode_7, .keep_all = T),
    by = c("int2a_preint_partcode"="int2a_preint_partcode_7"))

#filter for data before a certain cutoff date - CAN COMMENT OUT THIS LINE IF NECESSARY
#all_days <- all_days %>% mutate(preint_date = as.Date(preint_date))
#cutoff_date <- as.Date("2025-11-28")

#all_days <- all_days %>% filter(preint_date <= cutoff_date)
#Check for duplicates
length(unique(all_days_2a$int2a_preint_partcode))
length(unique(all_days_2a$int2a_drug_community_code))



#write_csv(all_days, "/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/fMDA/all_days.csv")

#New df for analyzing Hb results
hb_results <- all_days %>% select(preint_partcode, drug_hb_value, drug_hb_result3, drug_hb_diff3, drug_hb_result4, drug_hb_diff4, drug_hb_result5, drug_hb_diff5, 
                                  drug_hb_result6, drug_hb_diff6, drug_hb_result7, drug_hb_diff7)

hb_results <- hb_results %>%
  mutate(hb_diff_final = coalesce(drug_hb_diff3, drug_hb_diff4,  drug_hb_diff5, 
                                  drug_hb_diff6,  drug_hb_diff7))


hb_results%>% filter(hb_diff_final >0) %>% pull(hb_diff_final) %>% summary()
hb_results%>% filter(hb_diff_final <0) %>% pull(hb_diff_final) %>% summary()



hb_results <- hb_results %>% mutate(hb_diff_final = as.numeric(hb_diff_final))
sum(is.na(hb_results$hb_diff_final))
sum(!is.na(hb_results$hb_diff_final))


ggplot(hb_results, aes(y = hb_diff_final)) + 
  geom_boxplot()+
  labs(y = "Change in hemoglobin (g/dL)")+
  theme_light()

#create a new anemia categorical variable
hb_results$drug_hb_value <- as.numeric(hb_results$drug_hb_value)
hb_results$anemia_cat <- 
  #ifelse(basal$base_ind_sex == 'Female',
  cut(
    hb_results$drug_hb_value,
    breaks = c(-Inf, 8, 11, 12, Inf),  # Define the breakpoints
    labels = c( "Severe", "Moderate", "Mild", "Normal"),  # Define the labels
    right = FALSE  # Specify if the intervals are closed on the left or right
  )

#Checks to confirm variable is defined correctly
hb_results %>%  filter(anemia_cat == "Severe") %>%  summarize(
  min = min(drug_hb_value, na.rm = TRUE),
  max = max(drug_hb_value, na.rm = TRUE),
  n = n()  )
hb_results %>%  filter(anemia_cat == "Moderate") %>%  summarize(
  min = min(drug_hb_value, na.rm = TRUE),
  max = max(drug_hb_value, na.rm = TRUE),
  n = n()  )
hb_results %>%  filter(anemia_cat == "Mild") %>%  summarize(
  min = min(drug_hb_value, na.rm = TRUE),
  max = max(drug_hb_value, na.rm = TRUE),
  n = n()  )
hb_results %>%  filter(anemia_cat == "Normal") %>%  summarize(
  min = min(drug_hb_value, na.rm = TRUE),
  max = max(drug_hb_value, na.rm = TRUE),
  n = n()  )

table(hb_results$anemia_cat)



#New df for analyzing urine results
urine_results <- all_days %>% select(preint_partcode, drug_urine_result3, drug_urine_result4, drug_urine_result5, drug_urine_result6, drug_urine_result7 )

urine_results <- urine_results %>% mutate(urine_result_final = coalesce(drug_urine_result3, drug_urine_result4, drug_urine_result5, drug_urine_result6, drug_urine_result7))
urine_results$urine_result_final <- factor(urine_results$urine_result_final, levels = c(1, 2), labels = c("Positive", "Negative"))
table(urine_results$urine_result_final, useNA = "always")

#New df for analyzing symptoms
symptoms <- all_days %>% select(preint_partcode, drug_medications, drug_side_effects2:drug_other_adverse_effect3,drug_side_effects4:drug_other_adverse_effect4,
                                drug_side_effects5:drug_other_adverse_effect5, drug_side_effects6:drug_other_adverse_effect6,
                                drug_side_effects7:drug_other_adverse_effect7)

#symptoms <- symptoms %>%
# mutate(across(
#  c(drug_side_effects3, drug_side_effects_specify3___99,
#   drug_side_effects4, drug_side_effects_specify4___99,
#  drug_side_effects5, drug_side_effects_specify5___99,
# drug_side_effects6, drug_side_effects_specify6___99,
#drug_side_effects7, drug_side_effects_specify7___99), as.numeric))


#create a new variable that is 1 if pt said "yes" to experiencing symptoms any day over the course of their treatment
symptoms <- symptoms %>% 
  mutate(symptoms_anyday = if_else(if_any(drug_side_effects2|drug_side_effects3| drug_side_effects4|
                                            drug_side_effects5| drug_side_effects6| 
                                            drug_side_effects7,~ .x == 1), 1, 0))



#See how many people had at least 1 symptom any day
table(symptoms$symptoms_anyday, useNA = "always")

#how many people per treatment regimen had a symptom
symptoms %>%
  tabyl(symptoms_anyday, drug_medications) %>%
  adorn_totals("both") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()

#filter symptom dataset for only people that have symptom at least one day
symptoms <- symptoms %>% filter(symptoms_anyday == 1 )

symptoms <- symptoms %>% mutate(across(c(
  drug_side_effects_specify2___1,drug_side_effects_specify3___1,drug_side_effects_specify4___1,drug_side_effects_specify5___1, drug_side_effects_specify6___1, drug_side_effects_specify7___1,
  drug_side_effects_specify2___2,drug_side_effects_specify3___2, drug_side_effects_specify4___2, drug_side_effects_specify5___2, drug_side_effects_specify6___2,drug_side_effects_specify7___2,
  drug_side_effects_specify2___3,drug_side_effects_specify3___3, drug_side_effects_specify4___3,drug_side_effects_specify5___3, drug_side_effects_specify6___3,drug_side_effects_specify7___3,
  drug_side_effects_specify2___4,drug_side_effects_specify3___4, drug_side_effects_specify4___4,drug_side_effects_specify5___4, drug_side_effects_specify6___4,drug_side_effects_specify7___4,
  drug_side_effects_specify2___5,drug_side_effects_specify3___5, drug_side_effects_specify4___5,drug_side_effects_specify5___5, drug_side_effects_specify6___5, drug_side_effects_specify7___5,
  drug_side_effects_specify2___6,drug_side_effects_specify3___6, drug_side_effects_specify4___6,drug_side_effects_specify5___6, drug_side_effects_specify6___6,drug_side_effects_specify7___6,
  drug_side_effects_specify2___7,drug_side_effects_specify3___7, drug_side_effects_specify4___7,drug_side_effects_specify5___7, drug_side_effects_specify6___7,drug_side_effects_specify7___7,
  drug_side_effects_specify2___8,drug_side_effects_specify3___8, drug_side_effects_specify4___8,drug_side_effects_specify5___8, drug_side_effects_specify6___8, drug_side_effects_specify7___8, 
  drug_side_effects_specify2___9,drug_side_effects_specify3___9, drug_side_effects_specify4___9,drug_side_effects_specify5___9, drug_side_effects_specify6___9, drug_side_effects_specify7___9,
  drug_side_effects_specify2___10,drug_side_effects_specify3___10, drug_side_effects_specify4___10,drug_side_effects_specify5___10, drug_side_effects_specify6___10,drug_side_effects_specify7___10,
  drug_side_effects_specify2___99,drug_side_effects_specify3___99, drug_side_effects_specify4___99,drug_side_effects_specify5___99, drug_side_effects_specify6___99, drug_side_effects_specify7___99 ),
  as.numeric))




#sum the columns and store as values
dark_urine <- symptoms %>%
  summarise(dark_urine = sum(across(ends_with("___1")), na.rm = TRUE)) %>% pull(dark_urine)
jaundice <- symptoms %>%
  summarise(jaundice = sum(across(ends_with("___2")), na.rm = TRUE)) %>% pull(jaundice)
paleness <- symptoms %>%
  summarise(paleness = sum(across(ends_with("___3")), na.rm = TRUE)) %>% pull(paleness)
fatigue <- symptoms %>%
  summarise(fatigue = sum(across(ends_with("___4")), na.rm = TRUE)) %>% pull(fatigue)
dizziness <- symptoms %>%
  summarise(dizziness = sum(across(ends_with("___5")), na.rm = TRUE)) %>% pull(dizziness)
sob <- symptoms %>%
  summarise(sob = sum(across(ends_with("___6")), na.rm = TRUE)) %>% pull(sob)
back_pain<- symptoms %>%
  summarise(back_pain = sum(across(ends_with("___7")), na.rm = TRUE)) %>% pull(back_pain)
tachycardia <- symptoms %>%
  summarise(tachycardia = sum(across(ends_with("___8")), na.rm = TRUE)) %>% pull(tachycardia)
nausea <- symptoms %>%
  summarise(nausea = sum(across(ends_with("___9")), na.rm = TRUE)) %>% pull(nausea)
fever <- symptoms %>%
  summarise(fever = sum(across(ends_with("___10")), na.rm = TRUE)) %>% pull(fever)
other <- symptoms %>%
  summarise(other = sum(across(ends_with("___99")), na.rm = TRUE)) %>% pull(other)



#Df for vomit data
vomit <- all_days %>% select(preint_partcode, drug_medications, drug_vomit_15mins_1, drug_vomit_30mins_1, drug_vomit_15mins_2, drug_vomit_30mins_2,
                             drug_vomit_15mins_3, drug_vomit_30mins_3, drug_vomit_15mins_4, drug_vomit_30mins_4,
                             drug_vomit_15mins_5, drug_vomit_30mins_5, drug_vomit_15mins_6, drug_vomit_30mins_6, 
                             drug_vomit_15mins_7, drug_vomit_30mins_7)


vomit <- vomit %>%
  mutate(vomit15 = if_else(if_any(drug_vomit_15mins_1| drug_vomit_15mins_2|
                                    drug_vomit_15mins_3| drug_vomit_15mins_4| 
                                    drug_vomit_15mins_5|drug_vomit_15mins_6|drug_vomit_15mins_1,~ .x == 1), 1, 0),
         vomit30 = if_else(if_any(drug_vomit_30mins_1| drug_vomit_30mins_2|
                                    drug_vomit_30mins_3| drug_vomit_30mins_4| 
                                    drug_vomit_30mins_5|drug_vomit_30mins_6|drug_vomit_30mins_7,~ .x == 1), 1, 0))

table(vomit$vomit15, useNA = "always")
table(vomit$vomit30, useNA = "always")




### Numbers of people 

#How many people are eligible per community 
df_baseline <- read_csv("/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Baseline visit/IND and HH Surveys/df_baseline.csv")
elegibility_acsa <- read_csv("/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Organization/fMDA/Eligibility/eligibility_acsa.csv")
df_numbers <- df_baseline %>% 
  filter(community %in% c("01", "04", "06", "08", "09", "12", "14", "15", "17", "22", "23", "24", "25", "26", "29")) %>%
  group_by(community) %>% summarise(n_enrolled = n()) %>%
  left_join(elegibility_acsa %>% 
              group_by(Codigo_de_Comunidad) %>% 
              summarise(n_eligible = n()), 
            by = c("community"="Codigo_de_Comunidad")
  )

#How many people received at least one dose per community
all_days %>% group_by(drug_community_code)  %>% summarise(n_received = n()) %>% View()

df_numbers <- df_numbers %>% left_join(all_days %>% 
                                         group_by(drug_community_code)  %>% 
                                         summarise(n_received = n()),
                                       by = c("community"="drug_community_code")
)



df_numbers <- df_numbers %>% mutate(pct_received = (n_received/n_eligible)*100)




ggplot(df_numbers, aes(x = community, y=pct_received)) + 
  geom_bar(stat = "identity", color="black", fill="light blue")+
  geom_text(aes(label = round(pct_received, 1)), 
            vjust = -0.5, size = 3.0) +
  labs(x="Community", y = "Percent received 1d fMDA")+
  ylim(0,100)+
  theme_minimal()
# theme(axis.title = element_text(size=14,face="bold"),
#  axis.text.y = element_text(size = 13), 
# axis.text.x = element_text(size=13, angle = 45, hjust = 1))


df_numbers_long <- df_numbers %>%
  pivot_longer(
    cols = c(n_enrolled:n_received),
    names_to = "variable",
    values_to = "value"
  )

df_numbers_long$variable <- factor(df_numbers_long$variable,
                                   levels = c("n_enrolled", "n_eligible", "n_received"))


ggplot(df_numbers_long, aes(x = community, y = value, fill = variable)) +
  geom_bar(
    stat = "identity",
    position = "identity",
    alpha = 0.5  # transparency to see overlap
  ) +
  theme_minimal()




#write_csv(all_days, "/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/fMDA/all_days.csv")




# Review df 1 ----
df_intervention_day_1 %>% 
  filter(redcap_repeat_instrument=="intervencion_day_1") %>% 
  filter(preint_apprchyn=="1") %>% View() # se espera que solo 1 tenga abordaje (administracion) pero podrian no cumplir criterios
# count(drug_eligibility_review_conclus) %>% 
# filter(drug_day_of_intervention_1=="1") %>%
# add_count(preint_partcode) %>% 
# filter(n>1) %>% 
# count(preint_partcode) %>%
# count(redcap_repeat_instrument)
# select()
#View()
#left_join(
#  df_intervention_day_2 %>% 
#  filter(drug_day_of_intervention_2=="1") %>%
#  distinct(preint_partcode_2, .keep_all = T),
#  by = c("preint_partcode"="preint_partcode_2")
# count(preint_partcode_2) %>% View()
#%>% View()



no_appr <- df_intervention_day_1 %>% 
  filter(redcap_repeat_instrument=="intervencion_day_1") %>% 
  filter(preint_apprchyn=="2")


no_appr_wide <- no_appr %>%
  group_by(preint_partcode) %>%
  mutate(reason_number = row_number()) %>%
  pivot_wider(
    id_cols = preint_partcode,
    names_from = reason_number,
    values_from = preint_norsnother,
    names_prefix = "reason_"
  )

