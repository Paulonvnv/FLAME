setwd("~/Desktop/FLAME")

noti<-read.csv("localidades_noti.csv")

comm_location<-read.csv("test ubicaciones.csv")

n_pop<-read.csv("hugo.csv")



names(noti)

# [1] "LOC_DIS_mod"                  "test1"                        "test2"                        "LOCALIDAD_mod"               
# [5] "DISTRITO_mod"                 "PROVINCIA_mod"                "LOCALIDAD"                    "DISTRITO"                    
# [9] "PROVINCIA"                    "MALARIA.P..FALCIPARUM.2013"   "MALARIA.POR.P..MALARIAE.2013" "MALARIA.POR.P..VIVAX.2013"   
# [13] "Total.general.2013"           "MALARIA.P..FALCIPARUM.2014"   "MALARIA.POR.P..MALARIAE.2014" "MALARIA.POR.P..VIVAX.2014"   
# [17] "Total.general.2014"           "MALARIA.P..FALCIPARUM.2015"   "MALARIA.POR.P..MALARIAE.2015" "MALARIA.POR.P..VIVAX.2015"   
# [21] "Total.general.2015"           "MALARIA.P..FALCIPARUM.2016"   "MALARIA.POR.P..MALARIAE.2016" "MALARIA.POR.P..VIVAX.2016"   
# [25] "Total.general.2016"           "MALARIA.P..FALCIPARUM.2017"   "MALARIA.POR.P..VIVAX.2017"    "Total.general.2017"          
# [29] "MALARIA.P..FALCIPARUM.2018"   "MALARIA.POR.P..VIVAX.2018"    "Total.general.2018"           "MALARIA.P..FALCIPARUM.2019"  
# [33] "MALARIA.POR.P..VIVAX.2019"    "Total.general.2019"

noti_2<-data.frame("LOC_DIS" = NULL, "LOCALIDAD" = NULL, "DISTRITO" = NULL,
                 "PROVINCIA" = NULL, "BASIN" = NULL, "Selected" = NULL,
                 "MALARIA.P..FALCIPARUM.2013" = NULL,
                 "MALARIA.POR.P..MALARIAE.2013" = NULL, "MALARIA.POR.P..VIVAX.2013" = NULL, "Total.general.2013" = NULL,
                 "MALARIA.P..FALCIPARUM.2014" = NULL, "MALARIA.POR.P..MALARIAE.2014" = NULL, "MALARIA.POR.P..VIVAX.2014" = NULL,
                 "Total.general.2014" = NULL, "MALARIA.P..FALCIPARUM.2015" = NULL, "MALARIA.POR.P..MALARIAE.2015" = NULL,
                 "MALARIA.POR.P..VIVAX.2015" = NULL, "Total.general.2015" = NULL, "MALARIA.P..FALCIPARUM.2016" = NULL,
                 "MALARIA.POR.P..MALARIAE.2016" = NULL, "MALARIA.POR.P..VIVAX.2016" = NULL, "Total.general.2016" = NULL,
                 "MALARIA.P..FALCIPARUM.2017" = NULL, "MALARIA.POR.P..VIVAX.2017" = NULL, "Total.general.2017" = NULL,
                 "MALARIA.P..FALCIPARUM.2018" = NULL, "MALARIA.POR.P..VIVAX.2018" = NULL, "Total.general.2018" = NULL,
                 "MALARIA.P..FALCIPARUM.2019" = NULL, "MALARIA.POR.P..VIVAX.2019" = NULL, "Total.general.2019" = NULL)


for(n in levels(noti$LOC_DIS_mod)){
  df_1<-data.frame("LOC_DIS" = NA, "LOCALIDAD" = NA, "DISTRITO" = NA,
                   "PROVINCIA" = NA, "BASIN" = NA, "Selected" = NA,
                   "MALARIA.P..FALCIPARUM.2013" = NA,
                   "MALARIA.POR.P..MALARIAE.2013" = NA, "MALARIA.POR.P..VIVAX.2013" = NA, "Total.general.2013" = NA,
                   "MALARIA.P..FALCIPARUM.2014" = NA, "MALARIA.POR.P..MALARIAE.2014" = NA, "MALARIA.POR.P..VIVAX.2014" = NA,
                   "Total.general.2014" = NA, "MALARIA.P..FALCIPARUM.2015" = NA, "MALARIA.POR.P..MALARIAE.2015" = NA,
                   "MALARIA.POR.P..VIVAX.2015" = NA, "Total.general.2015" = NA, "MALARIA.P..FALCIPARUM.2016" = NA,
                   "MALARIA.POR.P..MALARIAE.2016" = NA, "MALARIA.POR.P..VIVAX.2016" = NA, "Total.general.2016" = NA,
                   "MALARIA.P..FALCIPARUM.2017" = NA, "MALARIA.POR.P..VIVAX.2017" = NA, "Total.general.2017" = NA,
                   "MALARIA.P..FALCIPARUM.2018" = NA, "MALARIA.POR.P..VIVAX.2018" = NA, "Total.general.2018" = NA,
                   "MALARIA.P..FALCIPARUM.2019" = NA, "MALARIA.POR.P..VIVAX.2019" = NA, "Total.general.2019" = NA)
  df_1$LOC_DIS<-n
  df_1$LOCALIDAD<-noti[noti$LOC_DIS_mod==n,]$LOCALIDAD_mod[1]
  df_1$DISTRITO<-noti[noti$LOC_DIS_mod==n,]$DISTRITO_mod[1]
  df_1$PROVINCIA<-noti[noti$LOC_DIS_mod==n,]$PROVINCIA_mod[1]
  df_1$BASIN<-noti[noti$LOC_DIS_mod==n,]$BASIN[1]
  df_1$Selected<-noti[noti$LOC_DIS_mod==n,]$Selected[1]
  df_1$MALARIA.P..FALCIPARUM.2013<-sum(noti[noti$LOC_DIS_mod==n,]$MALARIA.P..FALCIPARUM.2013, na.rm = T)
  df_1$MALARIA.P..FALCIPARUM.2014<-sum(noti[noti$LOC_DIS_mod==n,]$MALARIA.P..FALCIPARUM.2014, na.rm = T)
  df_1$MALARIA.P..FALCIPARUM.2015<-sum(noti[noti$LOC_DIS_mod==n,]$MALARIA.P..FALCIPARUM.2015, na.rm = T)
  df_1$MALARIA.P..FALCIPARUM.2016<-sum(noti[noti$LOC_DIS_mod==n,]$MALARIA.P..FALCIPARUM.2016, na.rm = T)
  df_1$MALARIA.P..FALCIPARUM.2017<-sum(noti[noti$LOC_DIS_mod==n,]$MALARIA.P..FALCIPARUM.2017, na.rm = T)
  df_1$MALARIA.P..FALCIPARUM.2018<-sum(noti[noti$LOC_DIS_mod==n,]$MALARIA.P..FALCIPARUM.2018, na.rm = T)
  df_1$MALARIA.P..FALCIPARUM.2019<-sum(noti[noti$LOC_DIS_mod==n,]$MALARIA.P..FALCIPARUM.2019, na.rm = T)
  df_1$MALARIA.POR.P..MALARIAE.2013<-sum(noti[noti$LOC_DIS_mod==n,]$MALARIA.POR.P..MALARIAE.2013, na.rm = T) 
  df_1$MALARIA.POR.P..MALARIAE.2014<-sum(noti[noti$LOC_DIS_mod==n,]$MALARIA.POR.P..MALARIAE.2014, na.rm = T)
  df_1$MALARIA.POR.P..MALARIAE.2015<-sum(noti[noti$LOC_DIS_mod==n,]$MALARIA.POR.P..MALARIAE.2015, na.rm = T)
  df_1$MALARIA.POR.P..MALARIAE.2016<-sum(noti[noti$LOC_DIS_mod==n,]$MALARIA.POR.P..MALARIAE.2016, na.rm = T)
  df_1$MALARIA.POR.P..VIVAX.2013<-sum(noti[noti$LOC_DIS_mod==n,]$MALARIA.POR.P..VIVAX.2013, na.rm = T)
  df_1$MALARIA.POR.P..VIVAX.2014<-sum(noti[noti$LOC_DIS_mod==n,]$MALARIA.POR.P..VIVAX.2014, na.rm = T)
  df_1$MALARIA.POR.P..VIVAX.2015<-sum(noti[noti$LOC_DIS_mod==n,]$MALARIA.POR.P..VIVAX.2015, na.rm = T)
  df_1$MALARIA.POR.P..VIVAX.2016<-sum(noti[noti$LOC_DIS_mod==n,]$MALARIA.POR.P..VIVAX.2016, na.rm = T)
  df_1$MALARIA.POR.P..VIVAX.2017<-sum(noti[noti$LOC_DIS_mod==n,]$MALARIA.POR.P..VIVAX.2017, na.rm = T)
  df_1$MALARIA.POR.P..VIVAX.2018<-sum(noti[noti$LOC_DIS_mod==n,]$MALARIA.POR.P..VIVAX.2018, na.rm = T)
  df_1$MALARIA.POR.P..VIVAX.2019<-sum(noti[noti$LOC_DIS_mod==n,]$MALARIA.POR.P..VIVAX.2019, na.rm = T)
  df_1$Total.general.2013<-sum(noti[noti$LOC_DIS_mod==n,]$Total.general.2013, na.rm = T)
  df_1$Total.general.2014<-sum(noti[noti$LOC_DIS_mod==n,]$Total.general.2014, na.rm = T)
  df_1$Total.general.2015<-sum(noti[noti$LOC_DIS_mod==n,]$Total.general.2015, na.rm = T)
  df_1$Total.general.2016<-sum(noti[noti$LOC_DIS_mod==n,]$Total.general.2016, na.rm = T)
  df_1$Total.general.2017<-sum(noti[noti$LOC_DIS_mod==n,]$Total.general.2017, na.rm = T)
  df_1$Total.general.2018<-sum(noti[noti$LOC_DIS_mod==n,]$Total.general.2018, na.rm = T)
  df_1$Total.general.2019<-sum(noti[noti$LOC_DIS_mod==n,]$Total.general.2019, na.rm = T)
  noti_2<-rbind(noti_2,df_1)
  rm(df_1)
}

View(noti_2)

rm(n)

names(n_pop)

# [1] "LOC_DIST"   "test1"      "LOCALIDAD"  "DISTRITO"   "PROVINCIA"  "POBLACION"  "casos.2013" "casos.2014" "casos.2015" "casos.2016" "casos.2017"
# [12] "casos.2018" "casos.2019" "IPAs.2013"  "IPAs.2014"  "IPAs.2015"  "IPAs.2016"  "IPAs.2017"  "IPAs.2018"  "IPAs.2019"  "filtro"


n_pop_2<-data.frame("LOC_DIS" = NULL, "POBLACION" = NULL)


for(n in levels(n_pop$LOC_DIST)){
  df_1<-data.frame("LOC_DIS" = NA, "POBLACION" = NA)
  df_1$LOC_DIS<-n
  df_1$POBLACION<-max(n_pop[n_pop$LOC_DIST==n,]$POBLACION, na.rm = T)
  n_pop_2<-rbind(n_pop_2,df_1)
  rm(df_1)
}


n_pop_2[n_pop_2$POBLACION=="-Inf",]$POBLACION<-NA

# n_pop_2$filter<-"NO"
# 
# 
# for (n in levels(as.factor(as.character(n_pop[n_pop$filtro=="SI",]$LOC_DIST)))){
#   n_pop_2[n_pop_2$LOC_DIS==n,]$filter<-"SI"
# }

rm(n)

noti_3<-merge(x = noti_2, y = n_pop_2, by.x = "LOC_DIS", by.y = "LOC_DIS", all.x = T, all.y = F)

noti_3$IPA.P..FALCIPARUM.2013<-(noti_3$MALARIA.P..FALCIPARUM.2013/noti_3$POBLACION)*1000
noti_3$IPA.P..FALCIPARUM.2014<-(noti_3$MALARIA.P..FALCIPARUM.2014/noti_3$POBLACION)*1000
noti_3$IPA.P..FALCIPARUM.2015<-(noti_3$MALARIA.P..FALCIPARUM.2015/noti_3$POBLACION)*1000
noti_3$IPA.P..FALCIPARUM.2016<-(noti_3$MALARIA.P..FALCIPARUM.2016/noti_3$POBLACION)*1000
noti_3$IPA.P..FALCIPARUM.2017<-(noti_3$MALARIA.P..FALCIPARUM.2017/noti_3$POBLACION)*1000
noti_3$IPA.P..FALCIPARUM.2018<-(noti_3$MALARIA.P..FALCIPARUM.2018/noti_3$POBLACION)*1000
noti_3$IPA.P..FALCIPARUM.2019<-(noti_3$MALARIA.P..FALCIPARUM.2019/noti_3$POBLACION)*1000
noti_3$IPA.POR.P..MALARIAE.2013<-(noti_3$MALARIA.POR.P..MALARIAE.2013/noti_3$POBLACION)*1000
noti_3$IPA.POR.P..MALARIAE.2014<-(noti_3$MALARIA.POR.P..MALARIAE.2014/noti_3$POBLACION)*1000
noti_3$IPA.POR.P..MALARIAE.2015<-(noti_3$MALARIA.POR.P..MALARIAE.2015/noti_3$POBLACION)*1000
noti_3$IPA.POR.P..MALARIAE.2016<-(noti_3$MALARIA.POR.P..MALARIAE.2016/noti_3$POBLACION)*1000
noti_3$IPA.POR.P..VIVAX.2013<-(noti_3$MALARIA.POR.P..VIVAX.2013/noti_3$POBLACION)*1000
noti_3$IPA.POR.P..VIVAX.2014<-(noti_3$MALARIA.POR.P..VIVAX.2014/noti_3$POBLACION)*1000
noti_3$IPA.POR.P..VIVAX.2015<-(noti_3$MALARIA.POR.P..VIVAX.2015/noti_3$POBLACION)*1000
noti_3$IPA.POR.P..VIVAX.2016<-(noti_3$MALARIA.POR.P..VIVAX.2016/noti_3$POBLACION)*1000
noti_3$IPA.POR.P..VIVAX.2017<-(noti_3$MALARIA.POR.P..VIVAX.2017/noti_3$POBLACION)*1000
noti_3$IPA.POR.P..VIVAX.2018<-(noti_3$MALARIA.POR.P..VIVAX.2018/noti_3$POBLACION)*1000
noti_3$IPA.POR.P..VIVAX.2019<-(noti_3$MALARIA.POR.P..VIVAX.2019/noti_3$POBLACION)*1000
noti_3$IPA.Total.general.2013<-(noti_3$Total.general.2013/noti_3$POBLACION)*1000
noti_3$IPA.Total.general.2014<-(noti_3$Total.general.2014/noti_3$POBLACION)*1000
noti_3$IPA.Total.general.2015<-(noti_3$Total.general.2015/noti_3$POBLACION)*1000
noti_3$IPA.Total.general.2016<-(noti_3$Total.general.2016/noti_3$POBLACION)*1000
noti_3$IPA.Total.general.2017<-(noti_3$Total.general.2017/noti_3$POBLACION)*1000
noti_3$IPA.Total.general.2018<-(noti_3$Total.general.2018/noti_3$POBLACION)*1000
noti_3$IPA.Total.general.2019<-(noti_3$Total.general.2019/noti_3$POBLACION)*1000




names(comm_location)

# [1] "wkt_geom"              "LOC_DIST"              "test"                  "MNOMCP1..a.modificar." "DIST"                  "MNOMCP2"              
# [7] "UBIGEO"                "DEP"                   "PROV"                  "CODCP"                 "NOMCP"                 "CAPITAL"              
# [13] "CON_IE"                "NIVEL"                 "CPINEI2"               "CPINEI17"              "FUENTE_G"              "Z"                    
# [19] "XGD"                   "YGD"                   "Y_X_COORD" 



noti_3<-merge(x = noti_3, y = comm_location, by.x = "LOC_DIS", by.y = "LOC_DIST", all.x = T, all.y = F)



noti_3$coord<-paste(noti_3$XGD,noti_3$YGD, sep = "_")


library(reshape2)



df <- read.csv("MalariaCases2013-2019/localidades_v3.csv", stringsAsFactors = F)




df$spe_ye<-paste(df$species,df$year,sep = ".")

df_wide <- dcast(df,LOCALIDAD+DISTRITO+PROVINCIA+
                   XGD+YGD+dist_minutes_cat3+dist_minutes_cat2+
                  dist_minutes_cat1+dist_minutes_all+urban+landc+
                  cobVeg+cobVeg_simbol~ spe_ye, value.var="cases")



df_wide$coord<-paste(df_wide$XGD,df_wide$YGD, sep = "_")


df_wide<-df_wide[c(6:13,39)]


names(noti_3)

noti_3<-merge(x = noti_3, y = df_wide, by.x = "coord", by.y = "coord", all.x = T, all.y = F)



cases_fal_long1 <- melt(noti_3,
                  # ID variables - all the variables to keep but not split apart on
                  id.vars=c("LOC_DIS",
                            "LOCALIDAD",
                            "DISTRITO",
                            "PROVINCIA",
                            "POBLACION",
                            "BASIN",
                            "Selected",
                            "XGD",
                            "YGD","dist_minutes_cat3",
                            "dist_minutes_cat2",
                            "dist_minutes_cat1",
                            "dist_minutes_all",
                            "urban",
                            "landc",
                            "cobVeg",
                            "cobVeg_simbol"
                  ),
                  # The source columns
                  measure.vars=c("MALARIA.P..FALCIPARUM.2013",
                                 "MALARIA.P..FALCIPARUM.2014",
                                 "MALARIA.P..FALCIPARUM.2015",
                                 "MALARIA.P..FALCIPARUM.2016",
                                 "MALARIA.P..FALCIPARUM.2017",
                                 "MALARIA.P..FALCIPARUM.2018",
                                 "MALARIA.P..FALCIPARUM.2019"
                  ),
                  # Name of the destination column that will identify the original
                  # column that the measurement came from
                  variable.name="year",
                  value.name="cases"
)


cases_fal_long1$year<-as.character(cases_fal_long1$year)

cases_fal_long1[cases_fal_long1$year=="MALARIA.P..FALCIPARUM.2013",]$year<-"2013"
cases_fal_long1[cases_fal_long1$year=="MALARIA.P..FALCIPARUM.2014",]$year<-"2014"
cases_fal_long1[cases_fal_long1$year=="MALARIA.P..FALCIPARUM.2015",]$year<-"2015"
cases_fal_long1[cases_fal_long1$year=="MALARIA.P..FALCIPARUM.2016",]$year<-"2016"
cases_fal_long1[cases_fal_long1$year=="MALARIA.P..FALCIPARUM.2017",]$year<-"2017"
cases_fal_long1[cases_fal_long1$year=="MALARIA.P..FALCIPARUM.2018",]$year<-"2018"
cases_fal_long1[cases_fal_long1$year=="MALARIA.P..FALCIPARUM.2019",]$year<-"2019"


api_fal_long1 <- melt(noti_3,
                        # ID variables - all the variables to keep but not split apart on
                        id.vars=c("LOC_DIS"
                        ),
                        # The source columns
                        measure.vars=c("IPA.P..FALCIPARUM.2013",
                                       "IPA.P..FALCIPARUM.2014",
                                       "IPA.P..FALCIPARUM.2015",
                                       "IPA.P..FALCIPARUM.2016",
                                       "IPA.P..FALCIPARUM.2017",
                                       "IPA.P..FALCIPARUM.2018",
                                       "IPA.P..FALCIPARUM.2019"
                        ),
                        # Name of the destination column that will identify the original
                        # column that the measurement came from
                        variable.name="year",
                        value.name="api"
)


api_fal_long1$year<-as.character(api_fal_long1$year)

api_fal_long1[api_fal_long1$year=="IPA.P..FALCIPARUM.2013",]$year<-"2013"
api_fal_long1[api_fal_long1$year=="IPA.P..FALCIPARUM.2014",]$year<-"2014"
api_fal_long1[api_fal_long1$year=="IPA.P..FALCIPARUM.2015",]$year<-"2015"
api_fal_long1[api_fal_long1$year=="IPA.P..FALCIPARUM.2016",]$year<-"2016"
api_fal_long1[api_fal_long1$year=="IPA.P..FALCIPARUM.2017",]$year<-"2017"
api_fal_long1[api_fal_long1$year=="IPA.P..FALCIPARUM.2018",]$year<-"2018"
api_fal_long1[api_fal_long1$year=="IPA.P..FALCIPARUM.2019",]$year<-"2019"



fal_long<-cbind(cases_fal_long1,api_fal_long1$api)

fal_long$specie<-"P. falciparum"
names(fal_long)[20]<-"api"

cases_mal_long <- melt(noti_3,
                       # ID variables - all the variables to keep but not split apart on
                       id.vars=c("LOC_DIS",
                                 "LOCALIDAD",
                                 "DISTRITO",
                                 "PROVINCIA",
                                 "POBLACION",
                                 "BASIN",
                                 "Selected",
                                 "XGD",
                                 "YGD","dist_minutes_cat3",
                                 "dist_minutes_cat2",
                                 "dist_minutes_cat1",
                                 "dist_minutes_all",
                                 "urban",
                                 "landc",
                                 "cobVeg",
                                 "cobVeg_simbol"
                       ),
                       # The source columns
                       measure.vars=c("MALARIA.POR.P..MALARIAE.2013",
                                      "MALARIA.POR.P..MALARIAE.2014",
                                      "MALARIA.POR.P..MALARIAE.2015",
                                      "MALARIA.POR.P..MALARIAE.2016"
                       ),
                       # Name of the destination column that will identify the original
                       # column that the measurement came from
                       variable.name="year",
                       value.name="cases"
)


cases_mal_long$year<-as.character(cases_mal_long$year)

cases_mal_long[cases_mal_long$year=="MALARIA.POR.P..MALARIAE.2013",]$year<-"2013"
cases_mal_long[cases_mal_long$year=="MALARIA.POR.P..MALARIAE.2014",]$year<-"2014"
cases_mal_long[cases_mal_long$year=="MALARIA.POR.P..MALARIAE.2015",]$year<-"2015"
cases_mal_long[cases_mal_long$year=="MALARIA.POR.P..MALARIAE.2016",]$year<-"2016"




api_mal_long <- melt(noti_3,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("LOC_DIS"
                     ),
                     # The source columns
                     measure.vars=c("IPA.POR.P..MALARIAE.2013",
                                    "IPA.POR.P..MALARIAE.2014",
                                    "IPA.POR.P..MALARIAE.2015",
                                    "IPA.POR.P..MALARIAE.2016"
                     ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="year",
                     value.name="api"
)


api_mal_long$year<-as.character(api_mal_long$year)

api_mal_long[api_mal_long$year=="IPA.POR.P..MALARIAE.2013",]$year<-"2013"
api_mal_long[api_mal_long$year=="IPA.POR.P..MALARIAE.2014",]$year<-"2014"
api_mal_long[api_mal_long$year=="IPA.POR.P..MALARIAE.2015",]$year<-"2015"
api_mal_long[api_mal_long$year=="IPA.POR.P..MALARIAE.2016",]$year<-"2016"


mal_long<-cbind(cases_mal_long,api_mal_long$api)

mal_long$specie<-"P. malariae"
names(mal_long)[20]<-"api"


cases_viv_long <- melt(noti_3,
                       # ID variables - all the variables to keep but not split apart on
                       id.vars=c("LOC_DIS",
                                 "LOCALIDAD",
                                 "DISTRITO",
                                 "PROVINCIA",
                                 "POBLACION",
                                 "BASIN",
                                 "Selected",
                                 "XGD",
                                 "YGD","dist_minutes_cat3",
                                 "dist_minutes_cat2",
                                 "dist_minutes_cat1",
                                 "dist_minutes_all",
                                 "urban",
                                 "landc",
                                 "cobVeg",
                                 "cobVeg_simbol"
                       ),
                       # The source columns
                       measure.vars=c("MALARIA.POR.P..VIVAX.2013",
                                      "MALARIA.POR.P..VIVAX.2014",
                                      "MALARIA.POR.P..VIVAX.2015",
                                      "MALARIA.POR.P..VIVAX.2016",
                                      "MALARIA.POR.P..VIVAX.2017",
                                      "MALARIA.POR.P..VIVAX.2018",
                                      "MALARIA.POR.P..VIVAX.2019"
                       ),
                       # Name of the destination column that will identify the original
                       # column that the measurement came from
                       variable.name="year",
                       value.name="cases"
)


cases_viv_long$year<-as.character(cases_viv_long$year)

cases_viv_long[cases_viv_long$year=="MALARIA.POR.P..VIVAX.2013",]$year<-"2013"
cases_viv_long[cases_viv_long$year=="MALARIA.POR.P..VIVAX.2014",]$year<-"2014"
cases_viv_long[cases_viv_long$year=="MALARIA.POR.P..VIVAX.2015",]$year<-"2015"
cases_viv_long[cases_viv_long$year=="MALARIA.POR.P..VIVAX.2016",]$year<-"2016"
cases_viv_long[cases_viv_long$year=="MALARIA.POR.P..VIVAX.2017",]$year<-"2017"
cases_viv_long[cases_viv_long$year=="MALARIA.POR.P..VIVAX.2018",]$year<-"2018"
cases_viv_long[cases_viv_long$year=="MALARIA.POR.P..VIVAX.2019",]$year<-"2019"

head(cases_viv_long)


api_viv_long <- melt(noti_3,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("LOC_DIS"
                     ),
                     # The source columns
                     measure.vars=c("IPA.POR.P..VIVAX.2013",
                                    "IPA.POR.P..VIVAX.2014",
                                    "IPA.POR.P..VIVAX.2015",
                                    "IPA.POR.P..VIVAX.2016",
                                    "IPA.POR.P..VIVAX.2017",
                                    "IPA.POR.P..VIVAX.2018",
                                    "IPA.POR.P..VIVAX.2019"
                     ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="year",
                     value.name="api"
)


api_viv_long$year<-as.character(api_viv_long$year)

api_viv_long[api_viv_long$year=="IPA.POR.P..VIVAX.2013",]$year<-"2013"
api_viv_long[api_viv_long$year=="IPA.POR.P..VIVAX.2014",]$year<-"2014"
api_viv_long[api_viv_long$year=="IPA.POR.P..VIVAX.2015",]$year<-"2015"
api_viv_long[api_viv_long$year=="IPA.POR.P..VIVAX.2016",]$year<-"2016"
api_viv_long[api_viv_long$year=="IPA.POR.P..VIVAX.2017",]$year<-"2017"
api_viv_long[api_viv_long$year=="IPA.POR.P..VIVAX.2018",]$year<-"2018"
api_viv_long[api_viv_long$year=="IPA.POR.P..VIVAX.2019",]$year<-"2019"

viv_long<-cbind(cases_viv_long,api_viv_long$api)

viv_long$specie<-"P. vivax"
names(viv_long)[20]<-"api"


cases_tot_long <- melt(noti_3,
                       # ID variables - all the variables to keep but not split apart on
                       id.vars=c("LOC_DIS",
                                 "LOCALIDAD",
                                 "DISTRITO",
                                 "PROVINCIA",
                                 "POBLACION",
                                 "BASIN",
                                 "Selected",
                                 "XGD",
                                 "YGD","dist_minutes_cat3",
                                 "dist_minutes_cat2",
                                 "dist_minutes_cat1",
                                 "dist_minutes_all",
                                 "urban",
                                 "landc",
                                 "cobVeg",
                                 "cobVeg_simbol"
                       ),
                       # The source columns
                       measure.vars=c("Total.general.2013",
                                      "Total.general.2014",
                                      "Total.general.2015",
                                      "Total.general.2016",
                                      "Total.general.2017",
                                      "Total.general.2018",
                                      "Total.general.2019"
                       ),
                       # Name of the destination column that will identify the original
                       # column that the measurement came from
                       variable.name="year",
                       value.name="cases"
)


cases_tot_long$year<-as.character(cases_tot_long$year)

cases_tot_long[cases_tot_long$year=="Total.general.2013",]$year<-"2013"
cases_tot_long[cases_tot_long$year=="Total.general.2014",]$year<-"2014"
cases_tot_long[cases_tot_long$year=="Total.general.2015",]$year<-"2015"
cases_tot_long[cases_tot_long$year=="Total.general.2016",]$year<-"2016"
cases_tot_long[cases_tot_long$year=="Total.general.2017",]$year<-"2017"
cases_tot_long[cases_tot_long$year=="Total.general.2018",]$year<-"2018"
cases_tot_long[cases_tot_long$year=="Total.general.2019",]$year<-"2019"

head(cases_tot_long)


api_tot_long <- melt(noti_3,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("LOC_DIS"
                     ),
                     # The source columns
                     measure.vars=c("IPA.Total.general.2013",
                                    "IPA.Total.general.2014",
                                    "IPA.Total.general.2015",
                                    "IPA.Total.general.2016",
                                    "IPA.Total.general.2017",
                                    "IPA.Total.general.2018",
                                    "IPA.Total.general.2019"
                     ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="year",
                     value.name="api"
)


api_tot_long$year<-as.character(api_tot_long$year)

api_tot_long[api_tot_long$year=="IPA.Total.general.2013",]$year<-"2013"
api_tot_long[api_tot_long$year=="IPA.Total.general.2014",]$year<-"2014"
api_tot_long[api_tot_long$year=="IPA.Total.general.2015",]$year<-"2015"
api_tot_long[api_tot_long$year=="IPA.Total.general.2016",]$year<-"2016"
api_tot_long[api_tot_long$year=="IPA.Total.general.2017",]$year<-"2017"
api_tot_long[api_tot_long$year=="IPA.Total.general.2018",]$year<-"2018"
api_tot_long[api_tot_long$year=="IPA.Total.general.2019",]$year<-"2019"

tot_long<-cbind(cases_tot_long,api_tot_long$api)

tot_long$specie<-"Total"
names(tot_long)[20]<-"api"


noti_long<-rbind(fal_long,mal_long,viv_long,tot_long)
head(noti_long)


write.csv(noti_long, file = "localidades_V7.csv")


