#Assessing which participants are in which datasets

library(dplyr)
library(REDCapR)
#library(tidyverse)
#library(ggplot2)
#library(readxl)
#library(purrr)


Sys.setenv(token_protocol = "E9135070AC82DFAAFA54E4B74BD1C127")


#Read in dataset with data from baseline survey- abordaje vivienda
abord <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 209923, guess_type = F)$data

#create abordaje with only hh that accepted
abord_accept <- abord %>% filter (appr_n_accpt > 0)

#Create new variable that concatenates comm code and house code
abord_accept$unihh <- paste0(abord_accept$appr_comm_id, abord_accept$appr_hh_code)   






#Read in dataset with data from baseline survey- household and individual level
house <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id =  209768, guess_type = F)$data
basal <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 209751, guess_type = F)$data

#elimnate rows where the indiv code is NA
house <- house %>% filter(!is.na(base_hh_house_code))
basal <- basal %>% filter(!is.na(base_ind_code))

#Create new variable that concatenates comm code and house code
house$unihh <- paste0(house$base_hh_com_code, house$base_hh_house_code)   





# Report from REDCap- intermediate forms
enc_int_hh <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 263201, guess_type = F)$data
enc_int_ind <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 263202, guess_type = F)$data

#Remove NA codes
enc_int_hh <- enc_int_hh %>% filter(!is.na(interim_hh_code))
enc_int_ind <- enc_int_ind %>% filter(!is.na(interim_ind_id))

#Create new variable that concatenates comm code and house code
enc_int_hh$unihh <- paste0(enc_int_hh$interim_hh_com_code, enc_int_hh$interim_hh_code)   





#Evaluate overlapping HH between forms
hh_abord <- unique(abord_accept$unihh) #1926
hh_basal <- unique(house$unihh) #1947
hh_int <- unique(enc_int_hh$unihh) #1693

all_three <- Reduce(intersect, list(hh_abord, hh_basal, hh_int)) #1669

only_abord <- setdiff(hh_abord, union(hh_basal, hh_int)) #2, there are 2 hh that are in abordaje df without any indiv survey
only_basal <- setdiff(hh_basal, union(hh_abord, hh_int)) #1, not good, there shouldn't be anyone in basal and not in baseline
  #maybe not an issue cuz they're in Palo Seco?
only_int <- setdiff(hh_int, union(hh_abord, hh_basal)) #2, this is not good





#Evaluate overlapping IND between forms
ind_basal <- unique(basal$base_ind_code) #4843
ind_int <- unique(enc_int_ind$interim_ind_id) #4282

both <- intersect(ind_basal, ind_int) #4281

only_basal_ind <- setdiff(ind_basal, ind_int) #716, this is ok
only_int_ind <- setdiff(ind_int, ind_basal) #1, this is not good, there shouldn't be anyone. This code is an error






#HOW MANY INDIV ACCEPTED PARTICIPATION IN ABORD COMPARED TO INDIV IN SURVEYS
#Make variables numeric
abord_accept <- abord_accept %>% mutate(across(c(appr_n_accpt, appr_n_rfs),as.numeric))

#Summary
abord_summary <- abord_accept %>%
  group_by(appr_comm_id) %>%
  summarise(
    total_accpt = sum(appr_n_accpt, na.rm = TRUE),
    total_rfs   = sum(appr_n_rfs, na.rm = TRUE),
    total_all   = total_accpt + total_rfs,
    house_accpt = n_distinct(appr_hh_code[appr_n_accpt > 0]),
    total_hh    = n_distinct(appr_hh_code),
    .groups = "drop"
  ) %>%
  arrange(appr_comm_id) %>%
  rename(Community = appr_comm_id)


sum(abord_summary$total_accpt) #4704, NOT GOOD, there are only 4704 indiv represented by total_accept in abord but there are far more surveys










