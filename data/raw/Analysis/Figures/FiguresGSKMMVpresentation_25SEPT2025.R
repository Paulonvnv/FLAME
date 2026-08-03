Load libraries
library(dplyr)
library(tidyr)
#library(magrittr)
library(tidyverse)
library(ggplot2)
#library(rmarkdown)
#library(gtsummary)
#library(gt)
library(openxlsx)
library(basecase)
library(readxl)




#Read in dataset with data from malaria cases
case <- read.csv('/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Analysis/Baseline visit/Malaria cases and costs/Tabla recolección de casos incidentes Promotor-Captador 22-ago-2025.csv')



#convert promotor and captador variables ->numeric
case$Promotor <- as.Date(case$Promotor, format = "%Y-%m-%d")  
case$Captador <- as.Date(case$Captador, format = "%Y-%m-%d")  

#create case date variable. if promotor is NA then captador is the date. if promotor is not NA, then keep promotor date
case <- case %>% mutate(case_date = if_else(is.na(Promotor) , Captador, Promotor))


case$month_year <- format(case$case_date, "%Y-%m")


ggplot(case %>% filter(!is.na(month_year)),
       aes(x = month_year)) +
  geom_bar()+
  labs(x = "Diagnosis Date",
       y = "Cases")+
  theme_light()
  




#Read in dataset with intv status
elig <- read.csv('/Users/sfine/Library/CloudStorage/Box-Box/FLAME/Mapping (post census)/Scripts/output/eligibility_list_slim_24SEP2025.csv')

# Ensure all pt_code values have 5 digits
elig$codigo_comuni_casa <- str_pad(elig$codigo_comuni_casa, width = 5, pad = "0")

#delete duplicated hh
elig_nodup <- elig %>% distinct(codigo_comuni_casa, .keep_all = TRUE)

#tab of intv status
#table(elig_nodup$comunidad, elig_nodup$estat_intv, useNA= "always")



#new var- if intervention status is case OR within intervention --> intervention 
elig_nodup <- elig_nodup %>%
  mutate(intervention_status = case_when(
    estat_intv %in% c("case", "within intervention") ~ "Intervention",
    is.na(estat_intv) ~ NA_character_,
    TRUE ~ "No intervention"  # keeps the original value for all other cases
  ))


elig_nodup <- elig_nodup %>%
mutate(comunidad = as.character(comunidad)) %>%
mutate(comm_name = case_when(
  comunidad == "1" ~ "Nina Rumi", 
  comunidad == "2" ~ "Zungarococha",
  comunidad == "3" ~ "Cahuide",
  comunidad == "4" ~ "13 de Febrero",
  comunidad == "5" ~ "Ex Petrolero",
 comunidad == "6" ~ "Samito",
 comunidad == "7" ~ "Diamante Azul",
 comunidad == "8" ~ "San Lucas",
 comunidad == "9" ~ "Moralillo",
 comunidad == "10" ~ "Pena Negra",
 comunidad == "11" ~ "Nuevo Horizonte",
 comunidad == "12" ~ "Libertad",
 comunidad == "13" ~ "Puerto Almendra",
 comunidad == "14" ~ "Tarapoto",
 comunidad == "15" ~ "Nuevo Milagro",
 comunidad == "16" ~ "San Antonio- SJB",
 comunidad == "17" ~ "Momoncillo",
 comunidad == "18" ~ "Paujil I",
 comunidad == "20" ~ "Huaturi",
 comunidad == "21" ~ "Salvador",
 comunidad == "22" ~ "Palo Seco",
 comunidad == "23" ~ "Nuevo San Antonio",
 comunidad == "24" ~ "Loboyacu",
 comunidad == "25" ~ "Santo Tomas de Capironal",
 comunidad == "26" ~ "Pisco",
 comunidad == "27" ~ "San Antonio-Punchana",
 comunidad == "28" ~ "Mishana",
 comunidad == "29" ~ "Fray Martin",
 comunidad == "31" ~ "Flor de Agosto",
 comunidad == "32" ~ "Almirante Miguel Grau"))


hh_summary <- elig_nodup %>%
  group_by(comm_name, intervention_status) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(comm_name) %>%
  mutate(pct = n / sum(n) * 100)  

  
  community_order_hh <- hh_summary %>% 
    filter(!is.na(intervention_status)) %>%
    group_by(comm_name) %>%
    summarise(intervention_pct = max(ifelse(intervention_status == "Intervention", pct, 0)), .groups = 'drop') %>%
    arrange(desc(intervention_pct)) %>%
    pull(comm_name)
  
  
  ## PLot in order from highest proportion targeted to lowest, n= HH targeted on the y axis
  ggplot(hh_summary %>% 
           filter(!is.na(intervention_status)) %>%
           mutate(comm_name = factor(comm_name, levels = community_order_hh)), 
         aes(x = comm_name, y = n, fill = intervention_status)) +
    geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
    geom_text(
      data = hh_summary %>% 
        filter(!is.na(intervention_status), intervention_status == "Intervention") %>%
        mutate(comm_name = factor(comm_name, levels = community_order_hh)),
      aes(label = paste0(round(pct, 1), "%")),
      position = position_stack(vjust = 0.5, reverse = TRUE),
      size = 2.5, 
      fontface = "bold") +
    scale_fill_manual(values = c("Intervention" = "#00B4D8", "No intervention" =  "#ADE8F4")) +
    labs(x = "Community", y = "Targeted households", fill = "Intervention status") +
    ylim(0,200)+
    theme_light()+
    theme(axis.text.x=element_text(angle = 45, vjust=1, hjust =1))
    

 ## PLot in order from most hh in a community to fewest hh in a community 

  #total_hh <- hh_summary %>% 
 #   group_by(comm_name) %>%
 #   summarise(total_n = sum(n, na.rm = TRUE), .groups = 'drop') %>%
 #   arrange(desc(total_n)) %>%
#   pull(comm_name)
  
  total_hh <- hh_summary %>% 
    group_by(comm_name) %>%
    summarise(
      total_n = sum(n, na.rm = TRUE),
      intervention_pct = max(ifelse(intervention_status == "Intervention", pct, 0), na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      scaled_hh = (total_n / max(total_n)) * 100,
      scaled_intv = intervention_pct,
      composite_score = (scaled_hh * 0.88) + (scaled_intv * 0.12)   # Equal weight
    ) %>%
    arrange(desc(composite_score)) %>%
    pull(comm_name)
  
  
  hh_summary<-hh_summary %>%
    mutate(intervention_status = case_when(
      is.na(intervention_status) ~ "Unknown",
      TRUE ~ intervention_status
    ))
  
  
  ggplot(hh_summary %>% 
          # filter(!is.na(intervention_status)) %>%
           mutate(comm_name = factor(comm_name, levels = total_hh)), 
         aes(x = comm_name, y = n, fill = intervention_status)) +
    geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
    geom_text(
      data = hh_summary %>% 
        filter(!is.na(intervention_status), intervention_status == "Intervention") %>%
        mutate(comm_name = factor(comm_name, levels = total_hh)),
      aes(label = paste0(round(pct, 1), "%")),
      position = position_stack(vjust = 0.5, reverse = TRUE),
      size = 2.5, 
      fontface = "bold") +
    scale_fill_manual(values = c("Intervention" = "#00B4D8", "No intervention" = "#ADE8F4", "Unknown" = "grey")) +
    scale_x_discrete(drop = FALSE) +  # This is key - keeps all factor levels
    labs(x = "Community", y = "Targeted households", fill = "Intervention status") +
    ylim(0, 200) +
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
    theme(axis.text = element_text(size = 14))+
    theme(axis.title = element_text(size = 14))+ 
    theme(legend.text = element_text(size = 14))+
    theme(legend.title = element_text(size = 14))
  
  
  
  
  
  
  
  ## PLot in order from highest proportion targeted to lowest, proportion targeted on the y axis
  ggplot(hh_summary %>% 
           filter(!is.na(intervention_status)) %>%
           mutate(comm_name = factor(comm_name, levels = community_order_hh)), 
         aes(x = comm_name, y = pct, fill = intervention_status)) +
    geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
    geom_text(
      data = hh_summary %>% 
        filter(!is.na(intervention_status), intervention_status == "Intervention") %>%
        mutate(comm_name = factor(comm_name, levels = community_order_hh)),
      aes(label = paste0(round(pct, 1), "%")),
      position = position_stack(vjust = 0.5, reverse = TRUE),
      size = 2.5, 
      fontface = "bold") +
    scale_fill_manual(values = c("Intervention" = "#00B4D8", "No intervention" =  "#ADE8F4", "Unknown"="lightgrey")) +
    labs(x = "Community", y = "Targeted households (%)", fill = "Intervention status") +
    theme_light()+
    theme(axis.text.x=element_text(angle = 45, vjust=1, hjust =1))+
    theme(axis.text = element_text(size = 14))+
    theme(axis.title = element_text(size = 14))+ 
    theme(legend.text = element_text(size = 14))+
    theme(legend.title = element_text(size = 14))
  
  
  
  
  
  
  
#BY INDIVIDUAL

#new var- if intervention status is case OR within intervention --> intervention 
elig_indiv <- elig %>%
  mutate(intervention_status = case_when(
    estat_intv %in% c("case", "within intervention") ~ "Intervention",
    is.na(estat_intv) ~ NA_character_,
    TRUE ~ "No intervention"  # keeps the original value for all other cases
  ))


indiv_summary <- elig_indiv %>%
  group_by(comunidad, intervention_status) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(comunidad) %>%
  mutate(pct = n / sum(n) * 100)



indiv_summary <- indiv_summary %>%
  mutate(comunidad = as.character(comunidad)) %>%
  mutate(comm_name = case_when(
    comunidad == "1" ~ "Nina Rumi", 
    comunidad == "2" ~ "Zungarococha",
    comunidad == "3" ~ "Cahuide",
    comunidad == "4" ~ "13 de Febrero",
    comunidad == "5" ~ "Ex Petrolero",
    comunidad == "6" ~ "Samito",
    comunidad == "7" ~ "Diamante Azul",
    comunidad == "8" ~ "San Lucas",
    comunidad == "9" ~ "Moralillo",
    comunidad == "10" ~ "Pena Negra",
    comunidad == "11" ~ "Nuevo Horizonte",
    comunidad == "12" ~ "Libertad",
    comunidad == "13" ~ "Puerto Almendra",
    comunidad == "14" ~ "Tarapoto",
    comunidad == "15" ~ "Nuevo Milagro",
    comunidad == "16" ~ "San Antonio- SJB",
    comunidad == "17" ~ "Momoncillo",
    comunidad == "18" ~ "Paujil I",
    comunidad == "20" ~ "Huaturi",
    comunidad == "21" ~ "Salvador",
    comunidad == "22" ~ "Palo Seco",
    comunidad == "23" ~ "Nuevo San Antonio",
    comunidad == "24" ~ "Loboyacu",
    comunidad == "25" ~ "Santo Tomas de Capironal",
    comunidad == "26" ~ "Pisco",
    comunidad == "27" ~ "San Antonio-Punchana",
    comunidad == "28" ~ "Mishana",
    comunidad == "29" ~ "Fray Martin",
    comunidad == "31" ~ "Flor de Agosto",
    comunidad == "32" ~ "Almirante Miguel Grau"))




community_order <- indiv_summary %>% 
  filter(!is.na(intervention_status)) %>%
  group_by(comm_name) %>%
  summarise(intervention_pct = max(ifelse(intervention_status == "Intervention", pct, 0)), .groups = 'drop') %>%
  arrange(desc(intervention_pct)) %>%
  pull(comm_name)


# Now plot with the correct ordering
ggplot(indiv_summary %>% 
         filter(!is.na(intervention_status)) %>%
         mutate(comm_name = factor(comm_name, levels = community_order)), 
       aes(x = comm_name, y = n, fill = intervention_status)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  geom_text(
    data = indiv_summary %>% 
      filter(!is.na(intervention_status), intervention_status == "Intervention") %>%
      mutate(comm_name = factor(comm_name, levels = community_order)),
    aes(label = paste0(round(pct, 1), "%")),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    size = 2.5, 
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("Intervention" = "#52a447" ,
               "No intervention" ="#acd8a7")) +
  labs(x = "Community", y = "Targeted individuals", fill = "Intervention status") +
  theme_light()+
  theme(axis.text.x=element_text(angle = 45, vjust=1, hjust =1))

