library(dplyr)
library(REDCapR)
library(lubridate)

# Report from REDCAp - FLAME for the base line forms

df_intervention_day_1 <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 241526L, guess_type = F)$data

df_intervention_day_2 <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 241527L, guess_type = F)$data

# Reviwew df 1 ----
df_intervention_day_1 %>% 
  filter(redcap_repeat_instrument=="intervencion_day_1") %>% 
  # filter(preint_apprchyn=="1") %>% # se espera que solo 1 tenga abordaje (administracion) pero podrian no cumplir criterios (157)
  # count(drug_eligibility_review_conclus) %>% 
  filter(drug_day_of_intervention_1=="1") %>% # (149)
  # add_count(preint_partcode) %>% 
  # filter(n>1) %>% 
  # count(preint_partcode) %>%
  # count(redcap_repeat_instrument)
  # select()
  # View()
  left_join(
    df_intervention_day_2 %>% 
      filter(drug_day_of_intervention_2=="1") %>%
      distinct(preint_partcode_2, .keep_all = T),
    by = c("preint_partcode"="preint_partcode_2")
            # count(preint_partcode_2) %>% View()
  ) %>% 
  View()
