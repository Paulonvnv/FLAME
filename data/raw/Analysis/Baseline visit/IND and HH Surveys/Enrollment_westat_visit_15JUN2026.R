
####INDIVIDUAL LEVEL####


#Read in dataset with data from baseline survey- individual level
basal <- REDCapR::redcap_report(redcap_uri = "https://redcap.ucsf.edu/api/", token = Sys.getenv("token_protocol"), report_id = 209751, guess_type = F)$data



#elimnate rows where the indiv code is NA
basal <- basal %>% filter(!is.na(base_ind_code))

#add labels to sex variable
basal$base_ind_sex <- factor(basal$base_ind_sex, levels = c(1, 2), labels = c("Male", "Female"))



# Ensure all base_ind_code values have 5 digits
basal$base_ind_code <- str_pad(basal$base_ind_code, width = 5, pad = "0")

#Create a new variable for community code
basal <- basal %>% mutate(community_code = substr(base_ind_code, 1, 2))



basal <- basal %>% select(community_code,base_ind_hh_code, base_ind_code, base_ind_date)
writexl::write_xlsx(basal, "/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Baseline visit/IND and HH Surveys/enrollment_westat_15JUN2026.xlsx")

