
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
previousdata%<>%
  filter(specie =="P. vivax")%>%
  pivot_wider(values_from ="cases", names_from = "year")

previousdata$`2019` = NULL

#previousdata%<>%select(names(previousdata)[!grepl("(pfal)|(pmal)|(malaria)|(2019)",names(previousdata))])

names(previousdata)=c("village_district",names(previousdata)[-1])

data_wide=left_join(data_wide,previousdata,by="village_district")

#data_wide%>%filter(is.na(specie))%>%ungroup%>%select(village_district)%>%unlist

new_names = data.frame(name_currentdata = c(
  "ANGUILLA_ALTO NANAY",
  "BUENAVISTA_ALTO NANAY",
  "SALVADOR (PAVA QUEVADA)_ALTO NANAY",
  "SANTA MARIA DE NANAY_ALTO NANAY", 
  "FRAY MARTIN_IQUITOS",
  "AGRARIO DE SHIMBILLO_PUNCHANA",
  "PROGRESO I ZONA_PUNCHANA",
  "SAN FERNANDO_PUNCHANA",
  "ANGEL CARDENAS_SAN JUAN BAUTISTA",
  "EX PETROLERO I ZONA_SAN JUAN BAUTISTA",
  "LOS DELFINES-CRUZ DEL SUR_SAN JUAN BAUTISTA",
  "NUEVA SANTA ELOYSA_SAN JUAN BAUTISTA",
  "PAUJIL II ZONA_SAN JUAN BAUTISTA",
  "PEÑA NEGRA_SAN JUAN BAUTISTA",
  "SAN PEDRO PINTUYACU_SAN JUAN BAUTISTA",
  "VILLA EL BUEN PASTOR_SAN JUAN BAUTISTA",
  "YUTO_SAN JUAN BAUTISTA",
  "ZUNGARO COCHA-CORRIENTILLO_SAN JUAN BAUTISTA"),
  name_previousdata=c("ANGUILLA_SAN JUAN BAUTISTA",
                      "BUENAVISTA / SAN PEDRO_ALTO NANAY",
                      "EL SALVADOR_ALTO NANAY",
                      "SANTA MARIA DEL ALTO NANAY_ALTO NANAY",
                      "FRAY MARTIN_PUNCHANA",
                      "AGRARIO SHIMBILLO_PUNCHANA",
                      "PROGRESO ZONA 1_PUNCHANA",
                      "NUEVO SAN FERNANDO_PUNCHANA",
                      "ANGEL CARDENAS HAYA I ZONA_SAN JUAN BAUTISTA",
                      "EX PETROLEROS I ZONA_SAN JUAN BAUTISTA",
                      "LOS DELFINES_SAN JUAN BAUTISTA",
                      "NUEVA SANTA ELOISA_SAN JUAN BAUTISTA",
                      "PAUJIL I ZONA_SAN JUAN BAUTISTA",
                      "PE\xd1A NEGRA_SAN JUAN BAUTISTA",
                      "SAN PEDRO DE PINTUYACU_SAN JUAN BAUTISTA",
                      "VILLA BUEN PASTOR_SAN JUAN BAUTISTA",
                      "SAN JUAN DE YUTO_SAN JUAN BAUTISTA",
                      "SANTA ISABEL DE ZUNGARO COCHA_SAN JUAN BAUTISTA"))

for (n in 1:nrow(new_names)) {
  data_wide[data_wide$village_district ==
              new_names[n, ][["name_currentdata"]],][,9:31] =
    previousdata[previousdata$village_district == new_names[n, ][["name_previousdata"]],][,-1]
}


library(raster)
library(ggplot2); library(dplyr);library(leaflet);library(sp); library(tmap); library(tmaptools); library(mapview)

names(data_wide) = c(
  "Province",
  "District",
  'Village',
  '2019',
  '2020',
  '2021',
  '2022',
  "village_district",
  "LOCALIDAD",
  "DISTRITO",
  "PROVINCIA",
  "Population size",
  "Basin",
  "Selected",
  "Latitude",
  "Longitude",
  "dist_minutes_cat3",
  "dist_minutes_cat2",
  "dist_minutes_cat1",
  "dist_minutes_all",
  "urban",
  "landc",
  "cobVeg",
  "cobVeg_simbol",
  "specie",
  "2013",
  "2014",
  "2015",
  "2016",
  "2017",
  "2018"
)

communities = data_wide %>% ungroup() %>% dplyr::select(village_district,
                                   Village,
                                   District,
                                   Province,
                                   Latitude,
                                   Longitude,
                                   `Population size`,
                                   dist_minutes_cat1,
                                   dist_minutes_cat3,
                                   `2021`,
                                   `2022`)


communities %<>% filter(!is.na(Latitude))

communities = communities[c(-7, -10, -20,-21,-22, -24, -25, -26, -67,-68,-69, -76, -88, -89, -100, -101, -102, -108),]

#ubicacion de los rios
rivers <- data.frame(River = c("Nanay river", "Pintuyacu river", "Momon river", "Nanay river"),
                     long = c(-73.90014, -73.71382, -73.38097, -73.351537),
                     lat = c(-3.861075, -3.730544, -3.574809,  -3.785743))

#ubicacion de iquitos
Iquitos <- data.frame(Community = c("Iquitos"),
                      long = c(-73.327286),
                      lat = c(-3.743489))
# texto de iquitos
Iquitos_text <- data.frame(Community = c("Iquitos"),
                           long = c(-73.327286),
                           lat = c(-4.25))

## Control Arm

coords <- communities[, c("Latitude", "Longitude")]   # coordinates
map_data   <- communities[,c('Village',
                         'District',
                         'Province',
                         'Latitude',
                         'Longitude',
                         'Population size',
                         'dist_minutes_cat1',
                         'dist_minutes_cat3',
                         '2021',
                         '2022')]          # data

crs    <- CRS("+init=epsg:4326") # proj4string of coords

communities_preselected <- SpatialPointsDataFrame(coords = coords,
                                                 data = map_data, 
                                                 proj4string = crs)

## numero de comunidad


# 
# names(communities)
coords <- communities[ , c("Latitude", "Longitude")]   # coordinates

communities$order = NA
pos = 1
for(commu in unique(communities$village_district)){
  communities[communities$village_district == commu, ][['order']] = pos
  pos = pos + 1
}


numb_data   <- communities[,c("order","Latitude", "Longitude")]          # data
crs    <- CRS("+init=epsg:4326") # proj4string of coords

communities_number_sp <- SpatialPointsDataFrame(coords = coords,
                                                data = numb_data,
                                                proj4string = crs)

##rios
coords <- rivers[ , c("long", "lat")]   # coordinates
crs    <- CRS("+init=epsg:4326") # proj4string of coords

rivers_sp <- SpatialPointsDataFrame(coords = coords,
                                    data = rivers, 
                                    proj4string = crs)

##iquitos
coords <- Iquitos[ , c("long", "lat")]   # coordinates
crs    <- CRS("+init=epsg:4326") # proj4string of coords
Iquitos_sp <- SpatialPointsDataFrame(coords = coords,
                                     data = Iquitos, 
                                     proj4string = crs)
##iquitos texto
coords <- Iquitos_text[ , c("long", "lat")]   # coordinates
crs    <- CRS("+init=epsg:4326") # proj4string of coords
Iquitos_text_sp <- SpatialPointsDataFrame(coords = coords,
                                          data = Iquitos_text, 
                                          proj4string = crs)

#geometria de loreto
loreto <- shapefile("../Limite_departamental/BAS_LIM_DEPARTAMENTO.shp")
loreto <- loreto[loreto@data$NOMBDEP == "LORETO",]


### Generacion de mapas
tmap_mode('view') #pone tmap en modo exploración
# mapa de las comunidades

p1 <- tm_shape(communities_preselected)+
  tm_dots("2022", size = 0.08, style="pretty", col = "lightblue")+
  #tm_shape(communities_preselected)+
  # tm_dots("api.malaria2019", size = 0.08, style="pretty", col = "red")+
  # tm_shape(communities_NoSelected)+
  #tm_dots(col="lightblue", size = 0.08)+
  tm_basemap('OpenStreetMap')+
  tm_shape(rivers_sp)+
  tm_text("River", size = 1.15)+
  tm_shape(communities_number_sp)+
  tm_text("order", size=0.6)+
  tm_scale_bar() 


p1

write.csv(communities, 'communitites.csv')




