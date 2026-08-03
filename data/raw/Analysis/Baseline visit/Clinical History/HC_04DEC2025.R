
library(dplyr)
library(REDCapR)
library(lubridate)
library(tidyverse)
library(ggplot2)
library (janitor)
library(gtsummary)

Sys.setenv(token_protocol = "E9135070AC82DFAAFA54E4B74BD1C127")


# Report from REDCap

hc <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 243863, guess_type = F)$data

hc <- hc %>% filter(!is.na(mhq_participant_code))
length(unique(hc$mhq_participant_code))



hc_filter <- hc %>% 
    filter (str_detect(tolower(mhq_diag_impr), "diabetes") |
            str_detect(tolower(mhq_diag_impr), "retin") |
            mhq_hc_diab == 1)


#Bring in basal DF
basal <- read.csv("/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Baseline visit/IND and HH Surveys/FLAME-Inddatareportsf_29MAY2025.csv")
basal <- basal %>% filter(!is.na(base_ind_code))


# Ensure all base_ind_code values have 5 digits
basal$base_ind_code <- str_pad(basal$base_ind_code, width = 5, pad = "0")

#Create a new variable for community code
basal <- basal %>% mutate(community_code = substr(base_ind_code, 1, 2))


basal <- basal %>% select(base_ind_code, community_code, base_ind_hh_code, base_ind_age, base_ind_age_2 )

#left join dfs
df <- left_join(hc_filter, basal, by = c("mhq_participant_code"="base_ind_code"))


#create current age variable
#convert bday variable character->numeric
df$mhq_bday <- as.Date(df$mhq_bday, format = "%Y-%m-%d")  

#calculate new age from birthday variable
today <- Sys.Date()
df$edad_actual <- as.numeric(difftime(today,df$mhq_bday, units = "weeks")) %/% 52



#reorder
df <- df %>% select(community_code, base_ind_hh_code, mhq_participant_code, mhq_bday, base_ind_age, base_ind_age_2, edad_actual,
                    mhq_sex,  mhq_meds_curr, mhq_meds_yes,  mhq_hc_diab, mhq_diag_impr )


#add labels 
df$mhq_sex <- factor(df$mhq_sex , levels = c(1, 2), labels = c("Hombre", "Mujer"))
df$mhq_hc_diab <- factor(df$mhq_hc_diab , levels = c(1, 2), labels = c("Diabetes", "No"))
df$mhq_meds_curr <- factor(df$mhq_meds_curr , levels = c(1, 2), labels = c("SI", "NO"))


#rename variables
df <- df %>%
  rename(
    Codigo_de_Comunidad = community_code,
    Codigo_casa = base_ind_hh_code,
    Codigo_participante = mhq_participant_code,
    Fecha_de_nacimiento = mhq_bday,
    Edad_anos_enrol = base_ind_age,
    Edad_meses_enrol = base_ind_age_2,
    Edad_acutal = edad_actual,
    Sexo = mhq_sex,
    Toma_med = mhq_meds_curr, 
    Med_espec = mhq_meds_yes,  
    Diabetes = mhq_hc_diab, 
    Diagnos = mhq_diag_impr 
    )


write_csv(df, "/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Baseline visit/Clinical History/informe_diag_med.csv")





