#FLAME participants with chronic diseases

library(dplyr)
library(REDCapR)
library(stringr)
library(tidyverse)
library(stringi)

#library(lubridate)
#library(ggplot2)
#library (janitor)
#library(gtsummary)

Sys.setenv(token_protocol = "E9135070AC82DFAAFA54E4B74BD1C127")


# Report from REDCap
hc <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 263997, guess_type = F)$data

#filter for missing pt code
hc <- hc %>% filter(!is.na(mhq_participant_code))

#select only variables I want
hc_select <- hc %>% select(mhq_participant_code, 
                    mhq_hc_anem, 
                    mhq_hc_transf, 
                    mhq_hc_tb, 
                    mhq_hc_asthma, 
                    mhq_hc_diab, 
                    mhq_hc_hepat, 
                    mhq_hc_bldstool, 
                    mhq_hc_cancer, 
                    mhq_hc_hemorr, 
                    mhq_hc_epil, 
                    mhq_hc_hypert, 
                    mhq_hc_liver, 
                    mhq_oth_cond, 
                    mhq_diag_impr)

#create new value for chronic columns
chronic_cols <- c(
  "mhq_hc_anem",
  "mhq_hc_transf",
  "mhq_hc_tb",
  "mhq_hc_asthma",
  "mhq_hc_diab",
  "mhq_hc_hepat",
  "mhq_hc_bldstool",
  "mhq_hc_cancer",
  "mhq_hc_hemorr",
  "mhq_hc_epil",
  "mhq_hc_hypert",
  "mhq_hc_liver"
)

#flag if any of the columns are true
hc_select_2 <- hc_select %>%
  mutate(
    flag_y_n = if_any(all_of(chronic_cols), ~ as.character(.) == "1"),
    flag_oth = !is.na(mhq_oth_cond) &
      !str_to_lower(str_trim(mhq_oth_cond)) %in% c("", "ninguna", "ninguno", "ningun", "niguna", "sano", "normal", "mimguna", 
                                                   "niniguna", "nnguna", "ninnguna", "ningina", "no refiere", "mninguna", "ninfuna","nignuna",
                                                   "niinguna"),
    flag_diag = !is.na(mhq_diag_impr) &
      !str_to_lower(str_trim(mhq_diag_impr)) %in% c("", "ninguna", "ninguno", "ningun", "niguna", "sano", "normal")
  ) %>%
  filter(flag_y_n | flag_oth | flag_diag)

#create community variable 
hc_select_2 <- hc_select_2 %>% mutate(community = substr(mhq_participant_code, 1, 2))


#bring in comm numbers
comm_codes <- read_csv("/Users/sfine/Library/CloudStorage/OneDrive-UCSF/Desktop/Quick access files/Community numbers_10APR2026.csv")

comm_codes <- comm_codes %>%
  mutate(brazo = if_else(
    Codigo %in% c("01", "04", "06", "08", "09", "12", "14", "15", "17", "22", "23", "24", "25", "26", "29"),
      "fMDA",
      "control"  ))


merge <- left_join(hc_select_2, comm_codes, by = c("community" = "Codigo"))



merge <- merge %>% 
rename(
  Codigo_de_Comunidad = community,
  Nombre_de_Comunidad = Comunidad,
  Codigo_de_Participante = mhq_participant_code,
  Anemia = mhq_hc_anem,
  Transfusion_de_sangre= mhq_hc_transf,
  Tuberculosis = mhq_hc_tb,
  Asma = mhq_hc_asthma,
  Diabetes = mhq_hc_diab,
  Hepatitis_o_renal= mhq_hc_hepat,
  Sangre_heces= mhq_hc_bldstool,
  Cancer=mhq_hc_cancer,
  Hematoma=mhq_hc_hemorr,
  Convulsion=mhq_hc_epil,
  Hipertension=mhq_hc_hypert,
  Cirrosis=mhq_hc_liver,
  Otro_cond=mhq_oth_cond,
  Diagnostico = mhq_diag_impr)


anemia <- merge %>% select(Anemia,Diagnostico) %>% filter( Anemia == "1"|
  str_detect(
    stri_trans_general(Diagnostico, "Latin-ASCII"),
    regex("anemia", ignore_case = TRUE)))


anemia <- anemia %>%
  mutate(
    anemia_grade = case_when(
      str_detect(Diagnostico, regex("anemia.*(grado\\s*1|\\b1\\b|\\bi\\b)", ignore_case = TRUE)) ~ "1",
      str_detect(Diagnostico, regex("anemia.*(grado\\s*2|\\b2\\b|\\bii\\b)", ignore_case = TRUE)) ~ "2",
      str_detect(Diagnostico, regex("anemia.*(grado\\s*3|\\b3\\b|\\biii\\b)", ignore_case = TRUE)) ~ "3",
      str_detect(Diagnostico, regex("anemia.*(grado\\s*4|\\b4\\b|\\biv\\b)", ignore_case = TRUE)) ~ "4",
      TRUE ~ "no_grade"))


diabetes <- merge %>% select(Diabetes,Diagnostico) %>% filter( Diabetes == "1" |
  str_detect(stri_trans_general(Diagnostico, "Latin-ASCII"),
    regex("diabet", ignore_case = TRUE)))


hiper <- merge %>% select(Hipertension,Diagnostico) %>% filter( Hipertension == "1" |
  str_detect(stri_trans_general(Diagnostico, "Latin-ASCII"),
    regex("hiperte", ignore_case = TRUE)))

Pterigion <- merge %>% select(Diagnostico) %>% filter(
  str_detect(stri_trans_general(Diagnostico, "Latin-ASCII"),
             regex("pter", ignore_case = TRUE)))

hipermet <- merge %>% select(Diagnostico) %>% filter(
  str_detect(stri_trans_general(Diagnostico, "Latin-ASCII"),
             regex("hipermet", ignore_case = TRUE)))


artrosis <- merge %>% select(Diagnostico) %>% filter(
  str_detect(stri_trans_general(Diagnostico, "Latin-ASCII"),
             regex("artro", ignore_case = TRUE)))



artritis <- merge %>% select(Diagnostico) %>% filter(
  str_detect(stri_trans_general(Diagnostico, "Latin-ASCII"),
             regex("artrit", ignore_case = TRUE)))



HIPERTIROIDISMO

HIPERMETROPIA



writexl::write_xlsx(merge, "/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Baseline visit/Clinical History/cond_sistem_06MAY2026.xlsx")


