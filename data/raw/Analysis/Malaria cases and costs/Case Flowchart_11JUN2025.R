
#Read in dataset with data from baseline survey- individual level
basal <- read.csv("FLAME-Inddatareportsf_29MAY2025.csv")

#elimnate rows where the indiv code is NA
basal <- basal %>% filter(!is.na(base_ind_code))

#new df with only ppl with fever at in past 48h at enrollment
fever_enroll <- basal %>% filter(base_ind_fever_48h ==1)


table(fever_enroll$base_ind_hlth_estab, useNA = "always")
table(fever_enroll$base_ind_gg_rslt, useNA = "always")


# Ensure all pt_code values have 5 digits
fever_enroll$base_ind_code <- str_pad(fever_enroll$base_ind_code, width = 5, pad = "0")


#read in case data
case <- read.csv("~/Library/CloudStorage/Box-Box/FLAME/Analysis/Baseline visit/Malaria cases and costs/FLAME-CasosdemalariaSF_30MAY2025.csv")

#elimnate rows where the indiv code is NA
case <- case %>% filter(!is.na(cc_participant_code))


# Ensure all pt_code values have 5 digits
case$cc_participant_code <- str_pad(case$cc_participant_code, width = 5, pad = "0")



#Merge case and fever enroll data together
#rename pt_code variables first
case <- case %>%
  rename(pt_code= cc_participant_code)

fever_enroll <- fever_enroll %>%
  rename(pt_code= base_ind_code)

#merge
case_fever <- 
  left_join(case, fever_enroll, by = c("pt_code"))



#Drop variables that aren't helpful 
case_fever <-  case_fever[,!is.element(names(case_fever),c("autonum", "redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance", "cost_interviewer",
"cc_residence_time"     ,             "cc_specify_years"      ,             "cc_specify_months"         ,        
 "cc_hh_head_travel"   ,               "cc_specify_hh_travel_times"  ,       "cc_hh_spend_night"         ,        
"cc_number_of_night"   ,              "cc_destination"            ,         "cc_reason_last_trip___1"     ,      
"cc_reason_last_trip___2"   ,         "cc_reason_last_trip___3"    ,        "cc_reason_last_trip___4"      ,     
 "cc_reason_last_trip___5"  ,          "cc_reason_last_trip___6"    ,        "cc_reason_last_trip___99"     ,     
 "cc_specify_other_reason"   ,         "cc_specify_work___0"         ,       "cc_specify_work___1"           ,    
"cc_specify_work___2"        ,        "cc_specify_work___3"           ,     "cc_specify_work___4"             ,  
 "cc_specify_work___5"         ,       "cc_specify_work___6"  ,              "cc_specify_work___7"             ,  
"cc_specify_work___8"          ,      "cc_specify_work___9"    ,            "cc_specify_work___99"              ,
 "cc_specify_other_work"         ,     "cc_transport___1"       ,            "cc_transport___2"               ,   
"cc_transport___3"     ,              "cc_transport___4"         ,          "cc_transport___99"                , 
 "cc_transport_other"    ,             "cc_sleep_loc___1"         ,          "cc_sleep_loc___2"                 , 
"cc_sleep_loc___3"       ,            "cc_sleep_loc___99"          ,        "cc_specify_other_place"            ,
 "cc_mosq_sleep"           ,           "registro_casos_de_malaria_complete", "cc_antimalarial_tx___1"  ,          
 "cc_antimalarial_tx___2"   ,          "cc_antimalarial_tx___3"    ,         "cc_antimalarial_tx___4" ,           
 "cc_antimalarial_tx___5"  ,           "cc_antimalarial_tx___6"  ,           "cc_antimalarial_tx___7" ,           
 "cc_antimalarial_tx___8"  ,           "cc_antimalarial_tx___99"  ,          "cc_other_tx_specify"     ))]


#merge the other way
fever_case <- 
  left_join(fever_enroll, case, by = c("pt_code"))





