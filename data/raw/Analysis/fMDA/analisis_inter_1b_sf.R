#Round 1b fMDA Analysis


library(dplyr)
library(REDCapR)
library(lubridate)
library(tidyverse)
library(ggplot2)
library (janitor)
library(gtsummary)

Sys.setenv(token_protocol = "E9135070AC82DFAAFA54E4B74BD1C127")


# Report from REDCAp - FLAME for the base line forms

df_intervention_day_1 <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 245971, guess_type = F)$data

df_intervention_day_2 <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 245972, guess_type = F)$data

df_intervention_day_3 <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 245973, guess_type = F)$data

df_intervention_day_4 <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 245974, guess_type = F)$data

df_intervention_day_5 <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 245975, guess_type = F)$data

df_intervention_day_6 <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 245976, guess_type = F)$data

df_intervention_day_7 <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 245977, guess_type = F)$data




#combine all dias of intervention, filtered where the drug day of intervention question for that day is ==1 
all_days_b <- df_intervention_day_1 %>% 
  filter(redcap_repeat_instrument=="intervencion_day_1_b") %>% 
  filter(b_drug_day_of_intervention_1=="1") %>%
  distinct(b_preint_partcode, .keep_all = T)%>%
  left_join(
    df_intervention_day_2 %>% 
      filter(b_drug_day_of_intervention_2=="1") %>%
      distinct(b_preint_partcode_2, .keep_all = T),
    by = c("b_preint_partcode"="b_preint_partcode_2")) %>% 
  left_join(
    df_intervention_day_3 %>% 
      filter(b_drug_day_of_intervention_3=="1")%>%
      distinct(b_preint_partcode_3, .keep_all = T),
    by = c("b_preint_partcode"="b_preint_partcode_3")) %>%
  
  left_join(
    df_intervention_day_4 %>% 
      filter(b_drug_day_of_intervention_4=="1")%>%
      distinct(b_preint_partcode_4, .keep_all = T),
    by = c("b_preint_partcode"="b_preint_partcode_4")) %>%
  left_join(
    df_intervention_day_5 %>% 
      filter(b_drug_day_of_intervention_5=="1")%>%
      distinct(b_preint_partcode_5, .keep_all = T),
    by = c("b_preint_partcode"="b_preint_partcode_5")) %>%
  left_join(
    df_intervention_day_6 %>% 
      filter(b_drug_day_of_intervention_6=="1")%>%
      distinct(b_preint_partcode_6, .keep_all = T),
    by = c("b_preint_partcode"="b_preint_partcode_6")) %>%
  left_join(
    df_intervention_day_7 %>% 
      filter(b_drug_day_of_intervention_7=="1")%>%
      distinct(b_preint_partcode_7, .keep_all = T),
    by = c("b_preint_partcode"="b_preint_partcode_7"))

#filter for data before a certain cutoff date - CAN COMMENT OUT THIS LINE IF NECESSARY
  #all_days <- all_days %>% mutate(preint_date = as.Date(preint_date))
  #cutoff_date <- as.Date("2025-11-28")
  #all_days <- all_days %>% filter(preint_date <= cutoff_date)


#Check for duplicates
length(unique(all_days$b_preint_partcode))


#New df for analyzing symptoms
symptoms <- all_days %>% select(b_preint_partcode, b_drug_medications, b_drug_side_effects3:b_drug_other_adverse_effect3,b_drug_side_effects4: b_drug_other_adverse_effect4,
                                b_drug_side_effects5: b_drug_other_adverse_effect5,  b_drug_side_effects6: b_drug_other_adverse_effect6,
                                b_drug_side_effects7: b_drug_other_adverse_effect7)


#create a new variable that is 1 if pt said "yes" to experiencing symptoms any day over the course of their treatment
symptoms <- symptoms %>% 
  mutate(symptoms_anyday = if_else(if_any( b_drug_side_effects3|  b_drug_side_effects4|
                                             b_drug_side_effects5|  b_drug_side_effects6| 
                                             b_drug_side_effects7,~ .x == 1), 1, 0))

#See how many people had at least 1 symptom any day
table(symptoms$symptoms_anyday, useNA = "always")

#how many people per treatment regimen had a symptom
symptoms %>%
  tabyl(symptoms_anyday,  b_drug_medications) %>%
  adorn_totals("both") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()



#create new symptom variables that are 1 if ANY day a given participant had that symptom, if not 0
symptoms <- symptoms %>%
  mutate(dark_urine = if_else(if_any( b_drug_side_effects_specify3___1|  b_drug_side_effects_specify4___1|
                                           b_drug_side_effects_specify5___1|  b_drug_side_effects_specify6___1| 
                                           b_drug_side_effects_specify7___1,~ .x == 1), 1, 0),
          jaundice = if_else(if_any( b_drug_side_effects_specify3___2|  b_drug_side_effects_specify4___2|
                                         b_drug_side_effects_specify5___2|  b_drug_side_effects_specify6___2| 
                                         b_drug_side_effects_specify7___2,~ .x == 1), 1, 0),
          paleness = if_else(if_any( b_drug_side_effects_specify3___3|  b_drug_side_effects_specify4___3|
                                         b_drug_side_effects_specify5___3|  b_drug_side_effects_specify6___3| 
                                         b_drug_side_effects_specify7___3,~ .x == 1), 1, 0),
         fatigue = if_else(if_any( b_drug_side_effects_specify3___4|  b_drug_side_effects_specify4___4|
                                     b_drug_side_effects_specify5___4|  b_drug_side_effects_specify6___4| 
                                     b_drug_side_effects_specify7___4,~ .x == 1), 1, 0),
         dizziness = if_else(if_any( b_drug_side_effects_specify3___5|  b_drug_side_effects_specify4___5|
                                       b_drug_side_effects_specify5___5|  b_drug_side_effects_specify6___5| 
                                       b_drug_side_effects_specify7___5,~ .x == 1), 1, 0),
         sob = if_else(if_any( b_drug_side_effects_specify3___6|  b_drug_side_effects_specify4___6|
                                 b_drug_side_effects_specify5___6|  b_drug_side_effects_specify6___6| 
                                 b_drug_side_effects_specify7___6,~ .x == 1), 1, 0),
         backpain = if_else(if_any( b_drug_side_effects_specify3___7|  b_drug_side_effects_specify4___7|
                                      b_drug_side_effects_specify5___7|  b_drug_side_effects_specify6___7| 
                                      b_drug_side_effects_specify7___7,~ .x == 1), 1, 0),
         tachycardia = if_else(if_any( b_drug_side_effects_specify3___8|  b_drug_side_effects_specify4___8|
                                         b_drug_side_effects_specify5___8|  b_drug_side_effects_specify6___8| 
                                         b_drug_side_effects_specify7___8,~ .x == 1), 1, 0),
         nausea_or_vomiting = if_else(if_any( b_drug_side_effects_specify3___9|  b_drug_side_effects_specify4___9|
                                                b_drug_side_effects_specify5___9|  b_drug_side_effects_specify6___9| 
                                                b_drug_side_effects_specify7___9,~ .x == 1), 1, 0),
         fever = if_else(if_any( b_drug_side_effects_specify3___10|  b_drug_side_effects_specify4___10|
                                   b_drug_side_effects_specify5___10|  b_drug_side_effects_specify6___10| 
                                   b_drug_side_effects_specify7___10,~ .x == 1), 1, 0),
         other = if_else(if_any( b_drug_side_effects_specify3___99|  b_drug_side_effects_specify4___99|
                                   b_drug_side_effects_specify5___99|  b_drug_side_effects_specify6___99| 
                                   b_drug_side_effects_specify7___99,~ .x == 1), 1, 0),
         
         other_symptom_spec = paste( b_drug_other_adverse_effect3,  b_drug_other_adverse_effect4,
                                     b_drug_other_adverse_effect5,  b_drug_other_adverse_effect6,
                                     b_drug_other_adverse_effect7, sep = "_")
         
  )

#number of people that had each of the following symptoms ANY day of intervention
sum(symptoms$dark_urine, na.rm= TRUE)
sum(symptoms$jaundice, na.rm= TRUE)
sum(symptoms$paleness, na.rm= TRUE)
sum(symptoms$fatigue, na.rm= TRUE)
sum(symptoms$dizziness, na.rm= TRUE)
sum(symptoms$sob, na.rm= TRUE)
sum(symptoms$backpain, na.rm= TRUE)
sum(symptoms$tachycardia, na.rm= TRUE)
sum(symptoms$fever, na.rm= TRUE)
sum(symptoms$nausea_or_vomiting, na.rm= TRUE)
sum(symptoms$other, na.rm= TRUE)

table(symptoms$other_symptom_spec)







#Df for vomit data
vomit <- all_days %>% select( b_preint_partcode,  b_drug_vomit_15mins_1, b_drug_vomit_30mins_1, b_drug_vomit_15mins_2, b_drug_vomit_30mins_2,
                              b_drug_vomit_15mins_3, b_drug_vomit_30mins_3, b_drug_vomit_15mins_4, b_drug_vomit_30mins_4,
                              b_drug_vomit_15mins_5, b_drug_vomit_30mins_5, b_drug_vomit_15mins_6, b_drug_vomit_30mins_6, 
                              b_drug_vomit_15mins_7, b_drug_vomit_30mins_7)


vomit <- vomit %>%
  mutate(vomit15 = if_else(if_any(b_drug_vomit_15mins_1| b_drug_vomit_15mins_2|
                                    b_drug_vomit_15mins_3| b_drug_vomit_15mins_4| 
                                    b_drug_vomit_15mins_5|b_drug_vomit_15mins_6|b_drug_vomit_15mins_1,~ .x == 1), 1, 0),
         vomit30 = if_else(if_any(b_drug_vomit_30mins_1| b_drug_vomit_30mins_2|
                                    b_drug_vomit_30mins_3| b_drug_vomit_30mins_4| 
                                    b_drug_vomit_30mins_5|b_drug_vomit_30mins_6|b_drug_vomit_30mins_7,~ .x == 1), 1, 0))

table(vomit$vomit15, useNA = "always")
table(vomit$vomit30, useNA = "always")



#Evaluate (non-required) hemoglobin testing and urine testing 
table(all_days$b_drug_hb_collect2)
table(all_days$b_drug_urine_collect2)





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


