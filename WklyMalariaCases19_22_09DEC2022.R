
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


#Plot p.vivax incidence in heat map by year and locality within district of Iquitos, adjusted breaks and labels for incidence and figure legend
report %>% filter(DISTRITO == 'IQUITOS',
                  LOCALIDAD != '(en blanco)',
                  parasite == 'P. vivax') %>%
  ggplot(aes(x = year,
             y = LOCALIDAD,
             fill = log(n_cases*3 + 1, 3)))+
  geom_tile()+
  scale_fill_gradient(low="white", high="red",
                      breaks = log(c(1,5,10,40,160)*3+1,3),
                      labels = c(1,5,10,40,160))+
  theme_bw()+
  labs(fill = 'N cases')
  



