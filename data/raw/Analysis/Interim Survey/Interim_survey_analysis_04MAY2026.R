#Intermediate Survey Analysis

library(dplyr)
library(REDCapR)
library(gtsummary)
library(tidyverse)



Sys.setenv(token_protocol = "E9135070AC82DFAAFA54E4B74BD1C127")


# Report from REDCap- intermediate forms

enc_int_hh <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 263201, guess_type = F)$data
enc_int_ind <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 263202, guess_type = F)$data

#Remove NA codes
enc_int_hh <- enc_int_hh %>% filter(!is.na(interim_hh_code))
enc_int_ind <- enc_int_ind %>% filter(!is.na(interim_ind_id))


# Ensure all participant code values have 5 digits
enc_int_ind$interim_ind_id <- str_pad(enc_int_ind$interim_ind_id, width = 5, pad = "0")

#Create a new variable for community code
enc_int_ind <- enc_int_ind %>% mutate(community_code = substr(interim_ind_id, 1, 2))


#Explore acceptability questions
table(enc_int_ind$interim_ind_rcv_fmda_yn)
table(enc_int_ind$interim_ind_fut_int_yn)

enc_int_ind <- enc_int_ind %>% 
  mutate(across(c(interim_ind_prtcp_why___1:interim_ind_prtcp_why___98),as.numeric)) %>% 
  rename (
  Es_mi_casa = interim_ind_prtcp_why___1, 
  Es_gratis = interim_ind_prtcp_why___2,
  Prevenir_malaria = interim_ind_prtcp_why___3, 
  Personal_amable = interim_ind_prtcp_why___4,
  Otro = interim_ind_prtcp_why___99, 
  No_sabe = interim_ind_prtcp_why___98
)


enc_int_ind <- enc_int_ind %>% 
  mutate(across(c(interim_ind_prtcp_whynt___1:interim_ind_prtcp_whynt___98),as.numeric)) %>% 
  rename (
  Mal_sabor = interim_ind_prtcp_whynt___1, 
  Meds_mix = interim_ind_prtcp_whynt___2,
  Efectos_sec = interim_ind_prtcp_whynt___3, 
  No_conf_equipo = interim_ind_prtcp_whynt___4, 
  Med_no_efic = interim_ind_prtcp_whynt___5, 
  No_enfermo = interim_ind_prtcp_whynt___6, 
  Explic_insuf = interim_ind_prtcp_whynt___7, 
  Dif_tom_pastilla = interim_ind_prtcp_whynt___8, 
  Med_sentir_mal = interim_ind_prtcp_whynt___9, 
  Demas_past = interim_ind_prtcp_whynt___10, 
  No_preguntas = interim_ind_prtcp_whynt___11, 
  Malaria_no_prob = interim_ind_prtcp_whynt___12, 
  Equip_ven_no_estoy = interim_ind_prtcp_whynt___13, 
  Otro_whynt = interim_ind_prtcp_whynt___99, 
  No_sabe_whynt = interim_ind_prtcp_whynt___98)



why_summary <- enc_int_ind %>%
  select(Es_mi_casa:No_sabe) %>%
  pivot_longer(
    cols = everything(),
    names_to = "reason",
    values_to = "selected") %>%
  filter(selected == 1) %>%
  count(reason, name = "N") %>%
  mutate(Percent = round(100 * N / sum(N), 1)) %>%
  arrange(desc(N))


whynt_summary <- enc_int_ind %>%
  select(Mal_sabor:No_sabe_whynt) %>%
  pivot_longer(
    cols = everything(),
    names_to = "reason",
    values_to = "selected") %>%
  filter(selected == 1) %>%
  count(reason, name = "N") %>%
  mutate(Percent = round(100 * N / sum(N), 1)) %>%
  arrange(desc(N))





