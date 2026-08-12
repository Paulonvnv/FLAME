library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(ggplot2)
control_malaria_cases_by_person=read.csv(file="C:/Users/brend/OneDrive/Escritorio/GitHub/FLAME/data/analysis/control_malaria_cases_by_person.csv")

# malaria_cases_long<-
#   pivot_longer(
#     control_malaria_cases_by_person,
#     cols = c("first_date",
#              "second_date","third_date","fourth_date","fifth_date",
#              "sixth_date","seventh_date","ninth_date","tenth_date"),
#     names_to = "episode",
#    # values_to = "date")
control_malaria_cases_by_person%<>%mutate(
  base_ind_code=case_when(
    nchar(base_ind_code)==4~as.character(paste0(0,base_ind_code)),
    nchar(base_ind_code)==5~as.character(base_ind_code)))
control_malaria_cases_by_person%<>%mutate(
  unihh_p=case_when(
    nchar(unihh_p)==4~as.character(paste0(0,unihh_p)),
    nchar(unihh_p)==5~as.character(unihh_p)))
control_malaria_cases_by_person%>%filter(is.na(base_ind_code))%>%select(unihh_p)%>%unlist()%>%duplicated()%>%sum()
control_malaria_cases_by_person%<>%mutate(base_ind_code=case_when(is.na(base_ind_code)~paste0("c",unihh_p),
                                                                  .default=base_ind_code))
control_malaria_cases_by_person%<>%mutate(village=gsub("(^c|\\d{3}$)","",base_ind_code))
malaria_cases_long<-pivot_longer(control_malaria_cases_by_person,cols = all_of(ends_with("_date")),names_to = "episode",values_to = "date")%>%filter(!is.na(date))

#505*13
#4545*6
895*5
class(malaria_cases_long$geometry)
options(digits = 16)
malaria_cases_long%<>%
  mutate(longitude=as.numeric(gsub("(^c\\(|,.+$)","",geometry)),
         latitude=as.numeric(gsub("(^.+,|\\)$)","",geometry)))
malaria_cases_long%<>%filter(!is.na(unihh_p))
listado_de_casas=unique(malaria_cases_long$unihh_p)
listado_de_casas=listado_de_casas[!is.na(listado_de_casas)]
casa1=listado_de_casas[1]
casa1
casa2
casa2=listado_de_casas[2]
casa3=listado_de_casas[3]
longitud1=malaria_cases_long%>%filter(unihh_p==casa1)%>%select(longitude)%>%unlist()%>%unique()
latitud1=malaria_cases_long%>%filter(unihh_p==casa1)%>%select(latitude)%>%unlist()%>%unique()
longitud2=malaria_cases_long%>%filter(unihh_p==casa2)%>%select(longitude)%>%unlist()%>%unique()
latitud2=malaria_cases_long%>%filter(unihh_p==casa2)%>%select(latitude)%>%unlist()%>%unique()
longitud3=malaria_cases_long%>%filter(unihh_p==casa3)%>%select(longitude)%>%unlist()%>%unique()
latitud3=malaria_cases_long%>%filter(unihh_p==casa3)%>%select(latitude)%>%unlist()%>%unique()
install.packages("geosphere")
library(geosphere)
point1=c(longitud1,latitud1)
point2=c(longitud2,latitud2)
point3=c(longitud3,latitud3)
distm(point1,point2,point3)
distHaversine(point1,point2,point3)
comparacion_entre_casas=as.data.frame(t(combn(listado_de_casas,2)))
colnames(comparacion_entre_casas)=c("casa_i","casa_j")
comparacion_entre_casas$dist_ij=NA
n=30
for (n in 1:nrow(comparacion_entre_casas)) {
  casai=comparacion_entre_casas[n,][["casa_i"]]
  casaj=comparacion_entre_casas[n,][["casa_j"]]
  longitude_i=unique(malaria_cases_long[malaria_cases_long$unihh_p==casai,][["longitude"]])
  latitude_i=unique(malaria_cases_long[malaria_cases_long$unihh_p==casai,][["latitude"]])                                              
  longitude_j=unique(malaria_cases_long[malaria_cases_long$unihh_p==casaj,][["longitude"]])
  latitude_j=unique(malaria_cases_long[malaria_cases_long$unihh_p==casaj,][["latitude"]])  
  point_i=c(longitude_i,latitude_i)
  point_j=c(longitude_j,latitude_j)
  comparacion_entre_casas[n,][["dist_ij"]]=distHaversine(point_i,point_j) 
}
casai
comparacion_entre_casas=left_join(comparacion_entre_casas,malaria_cases_long%>%select(unihh_p,village),by=join_by("casa_i"=="unihh_p"))
comparacion_entre_casas%<>%dplyr::rename("village_i"="village")
comparacion_entre_casas=left_join(comparacion_entre_casas,malaria_cases_long%>%select(unihh_p,village),
                                  by=join_by("casa_j"=="unihh_p"))
comparacion_entre_casas%<>%dplyr::rename("village_j"="village")
