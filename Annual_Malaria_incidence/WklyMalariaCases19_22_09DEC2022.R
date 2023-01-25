
library(dplyr)
library(tidyr)
library(magrittr)

data = read.csv('Comunidades_FLAME_18enero2023.csv')


##Convert data to long format
data_l = data %>% pivot_longer(cols = all_of(names(data)[-1:-7]),
                               names_to = 'year',
                               values_to = 'n_cases')

library(stringr)

#Extract only desired values from parasite and week variables
data_l$week = str_extract(data_l$year, '[0-9]+$')

data_l$year = gsub('X|_','',str_extract(data_l$year, 'X[0-9]+_'))

names(data_l)
#Collapse data by province, district, locality, year, parasite species
report = data_l %>% group_by(Province,
                    district,
                    Census.INEI.2017, year) %>%
  summarize(n_cases = sum(n_cases, na.rm = TRUE))

library(ggplot2)


#Filter data for only district=Iquitos, parasite=P. vivax
#Plot p.vivax incidence in heat map by year and locality within district of Iquitos
report %>%
  ggplot(aes(x = year,
             y = Census.INEI.2017,
             fill = n_cases))+
  geom_tile()+
  scale_fill_gradient(low="white", high="red",
                      #breaks = 0:5,
                      #labels = 3^(0:5)-1
                      )+
  theme_bw()+
  labs(fill = 'N cases')

data_wide=report%>%pivot_wider(values_from = "n_cases", names_from = "year")
data_wide%<>%mutate(village_district=paste(Census.INEI.2017,district,sep = "_"))
names(data_wide)=c(names(data_wide)[1:2],
"village",
names(data_wide)[4:8])

#combine the current number of cases with the previous data number of cases between 2012 and 2018, geographic location, distance from Iquitos, distance from the health center, density population, 

#previousdata=read.csv("selected_comm4.csv")  

previousdata = read.csv("../localidades_V7.csv")
previousdata$api=NULL
previousdata%<>%filter(specie =="P. vivax")%>%pivot_wider(values_from ="cases", names_from = "year")

#previousdata%<>%select(names(previousdata)[!grepl("(pfal)|(pmal)|(malaria)|(2019)",names(previousdata))])

names(previousdata)=c("village_district",names(previousdata)[-1])

data_wide=left_join(data_wide,previousdata,by="village_district")

data_wide%>%filter(is.na(specie))%>%ungroup%>%select(village_district)%>%unlist

new_names=data.frame(name_currentdata=c(
  "ANGUILLA_ALTO NANAY","BUENAVISTA_ALTO NANAY", "SALVADOR (PAVA QUEVADA)_ALTO NANAY",  "SANTA MARIA DE NANAY_ALTO NANAY", 
  "FRAY MARTIN_IQUITOS", "AGRARIO DE SHIMBILLO_PUNCHANA", "PROGRESO I ZONA_PUNCHANA",  "SAN FERNANDO_PUNCHANA",
  "ANGEL CARDENAS_SAN JUAN BAUTISTA", "EX PETROLERO I ZONA_SAN JUAN BAUTISTA" ,  "LOS DELFINES-CRUZ DEL SUR_SAN JUAN BAUTISTA",
  "NUEVA SANTA ELOYSA_SAN JUAN BAUTISTA","PAUJIL II ZONA_SAN JUAN BAUTISTA",  "PEÑA NEGRA_SAN JUAN BAUTISTA", "SAN PEDRO PINTUYACU_SAN JUAN BAUTISTA",
  "VILLA EL BUEN PASTOR_SAN JUAN BAUTISTA", "YUTO_SAN JUAN BAUTISTA", "ZUNGARO COCHA-CORRIENTILLO_SAN JUAN BAUTISTA"
),name_previousdata=c("ANGUILLA_SAN JUAN BAUTISTA", "BUENAVISTA / SAN PEDRO_ALTO NANAY", "EL SALVADOR_ALTO NANAY",
                      "SANTA MARIA DEL ALTO NANAY_ALTO NANAY", "FRAY MARTIN_PUNCHANA", "AGRARIO SHIMBILLO_PUNCHANA",
                     "PROGRESO ZONA 1_PUNCHANA", "NUEVO SAN FERNANDO_PUNCHANA", "ANGEL CARDENAS HAYA I ZONA_SAN JUAN BAUTISTA",
                     "EX PETROLEROS I ZONA_SAN JUAN BAUTISTA", "LOS DELFINES_SAN JUAN BAUTISTA", "NUEVA SANTA ELOISA_SAN JUAN BAUTISTA",
                    "PAUJIL I ZONA_SAN JUAN BAUTISTA", "PE<d1>A NEGRA_SAN JUAN BAUTISTA", "SAN PEDRO DE PINTUYACU_SAN JUAN BAUTISTA",
                    "VILLA BUEN PASTOR_SAN JUAN BAUTISTA", "SAN JUAN DE YUTO_SAN JUAN BAUTISTA", "SANTA ISABEL DE ZUNGARO COCHA_SAN JUAN BAUTISTA"))

for (n in 1:nrow(new_names)) {
  data_wide[data_wide$village_district==new_names[n, ][["name_currentdata"]],9:32] = previousdata[previousdata$village_district == new_names[n, ][["name_previousdata"]],-1]
}

