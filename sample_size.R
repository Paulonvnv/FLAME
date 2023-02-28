
# setwd("D:/UTSW/fMDA_Project/Randomization/Randomization1/")
# randomizationTable0 <- read.csv("selected_comm4.csv",sep=",",header=T)

randomizationTable0 = communities

# Data cleaning----
# remove duplicated rows
randomizationTable0<-randomizationTable0[!duplicated(randomizationTable0$village_district),]



randomizationTable0[is.na(randomizationTable0$`Population size`),][['Population size']] = c(63, 129, 227, 168, 177)


# function of the harmonic mean and its standard deviation
hmean<-function(x) {1/mean(1/x)}
sd.hmean<-function(x) {sqrt((mean(1/x))^(-4)*var(1/x)/length(x))}

#imputation of population size with the harmonic mean
randomizationTable0[is.na(randomizationTable0$`Population size`),][["Population size"]]<-ceiling(hmean(randomizationTable0[!is.na(randomizationTable0$`Population size`),][["Population size"]]))

#calculation of api for missing data

randomizationTable0 %<>% mutate(
  ipa.2021 = 1000*`2021`/`Population size`,
  ipa.2022 = 1000*`2022`/`Population size`
)


randomizationTable0[randomizationTable0$order == 2,][,c('dist_minutes_cat1','dist_minutes_cat3')] = randomizationTable0[randomizationTable0$order == 17,][,c('dist_minutes_cat1','dist_minutes_cat3')]
randomizationTable0[randomizationTable0$order == 15,][,c('dist_minutes_cat1','dist_minutes_cat3')] = randomizationTable0[randomizationTable0$order == 1,][,c('dist_minutes_cat1','dist_minutes_cat3')]
randomizationTable0[randomizationTable0$order == 20,][,c('dist_minutes_cat1','dist_minutes_cat3')] = randomizationTable0[randomizationTable0$order == 28,][,c('dist_minutes_cat1','dist_minutes_cat3')]
randomizationTable0[randomizationTable0$order == 19,][,c('dist_minutes_cat1','dist_minutes_cat3')] = randomizationTable0[randomizationTable0$order == 37,][,c('dist_minutes_cat1','dist_minutes_cat3')]
randomizationTable0[randomizationTable0$order == 26,][,c('dist_minutes_cat1','dist_minutes_cat3')] = randomizationTable0[randomizationTable0$order == 23,][,c('dist_minutes_cat1','dist_minutes_cat3')]
randomizationTable0[randomizationTable0$order == 30,][,c('dist_minutes_cat1','dist_minutes_cat3')] = randomizationTable0[randomizationTable0$order == 44,][,c('dist_minutes_cat1','dist_minutes_cat3')]
randomizationTable0[randomizationTable0$order == 33,][,c('dist_minutes_cat1','dist_minutes_cat3')] = randomizationTable0[randomizationTable0$order == 37,][,c('dist_minutes_cat1','dist_minutes_cat3')]
randomizationTable0[randomizationTable0$order == 19,][,c('dist_minutes_cat1','dist_minutes_cat3')] = randomizationTable0[randomizationTable0$order == 37,][,c('dist_minutes_cat1','dist_minutes_cat3')]
randomizationTable0[randomizationTable0$order == 42,][,c('dist_minutes_cat1','dist_minutes_cat3')] = randomizationTable0[randomizationTable0$order == 41,][,c('dist_minutes_cat1','dist_minutes_cat3')]
randomizationTable0[randomizationTable0$order == 54,][,c('dist_minutes_cat1','dist_minutes_cat3')] = randomizationTable0[randomizationTable0$order == 60,][,c('dist_minutes_cat1','dist_minutes_cat3')]
randomizationTable0[randomizationTable0$order == 74,][,c('dist_minutes_cat1','dist_minutes_cat3')] = randomizationTable0[randomizationTable0$order == 60,][,c('dist_minutes_cat1','dist_minutes_cat3')]
randomizationTable0[randomizationTable0$order == 58,][,c('dist_minutes_cat1','dist_minutes_cat3')] = randomizationTable0[randomizationTable0$order == 76,][,c('dist_minutes_cat1','dist_minutes_cat3')]
randomizationTable0[randomizationTable0$order == 59,][,c('dist_minutes_cat1','dist_minutes_cat3')] = randomizationTable0[randomizationTable0$order == 71,][,c('dist_minutes_cat1','dist_minutes_cat3')]
randomizationTable0[randomizationTable0$order == 78,][,c('dist_minutes_cat1','dist_minutes_cat3')] = randomizationTable0[randomizationTable0$order == 71,][,c('dist_minutes_cat1','dist_minutes_cat3')]
randomizationTable0[randomizationTable0$order == 63,][,c('dist_minutes_cat1','dist_minutes_cat3')] = randomizationTable0[randomizationTable0$order == 88,][,c('dist_minutes_cat1','dist_minutes_cat3')]
randomizationTable0[randomizationTable0$order == 79,][,c('dist_minutes_cat1','dist_minutes_cat3')] = randomizationTable0[randomizationTable0$order == 71,][,c('dist_minutes_cat1','dist_minutes_cat3')]
randomizationTable0[randomizationTable0$order == 80,][,c('dist_minutes_cat1','dist_minutes_cat3')] = randomizationTable0[randomizationTable0$order == 96,][,c('dist_minutes_cat1','dist_minutes_cat3')]
randomizationTable0[randomizationTable0$order == 84,][,c('dist_minutes_cat1','dist_minutes_cat3')] = randomizationTable0[randomizationTable0$order == 92,][,c('dist_minutes_cat1','dist_minutes_cat3')]
randomizationTable0[randomizationTable0$order == 90,][,c('dist_minutes_cat1','dist_minutes_cat3')] = randomizationTable0[randomizationTable0$order == 95,][,c('dist_minutes_cat1','dist_minutes_cat3')]
randomizationTable0[randomizationTable0$order == 98,][,c('dist_minutes_cat1','dist_minutes_cat3')] = randomizationTable0[randomizationTable0$order == 81,][,c('dist_minutes_cat1','dist_minutes_cat3')]
randomizationTable0[randomizationTable0$order == 99,][,c('dist_minutes_cat1','dist_minutes_cat3')] = randomizationTable0[randomizationTable0$order == 82,][,c('dist_minutes_cat1','dist_minutes_cat3')]

#selection of villages with api higher than 0 and lower than 250----

randomizationTable<-randomizationTable0[randomizationTable0$`2022`>2&
                                          randomizationTable0$ipa.2022<250&
                                          randomizationTable0$`Population size`<650&
                                          randomizationTable0$dist_minutes_cat3 <500,]

no_selected_highapi <-randomizationTable0[randomizationTable0$`2022`>2&
                                          randomizationTable0$ipa.2022 >= 250&
                                          randomizationTable0$`Population size`<650&
                                          randomizationTable0$dist_minutes_cat3 <500,]


library(geosphere)

# calculate distance in meters between villages----

# Calculate all pairs of polygons
combns <- t(combn(length(randomizationTable$Village), 2))

# For each row of combns, calculate Haus. dist. for the relevant pair of 
#  polygons
dists <- apply(combns, 1, function(x) 
  distm(randomizationTable[x[1],c("Longitude","Latitude")], randomizationTable[x[2],c("Longitude","Latitude")], fun = distGeo))


hdists <- cbind.data.frame(from=as.character(randomizationTable$Village[combns[, 1]]), 
                           to=as.character(randomizationTable$Village[combns[, 2]]), 
                           d=dists)

# histogram and summary table of selected variables----

par(mfrow=c(2,3))

hist(randomizationTable$`Population size`,breaks = 50, freq = T, xlab = "Population size", main = NULL)
hist(randomizationTable$ipa.2022,breaks = 50, freq = T, xlab = "IPA 2022", main = NULL)
hist(randomizationTable$`2022`,breaks = 50, freq = T, xlab = "Malaria cases 2022", main = NULL)
hist(randomizationTable$dist_minutes_cat3/60,breaks = 50, freq = T, xlab = "Distance to Iquitos (hs)", main = NULL)
hist(randomizationTable$dist_minutes_cat1,breaks = 50, freq = T, xlab = "Distance to HF (min)", main = NULL)
hist(hdists$d/1000,breaks = 50, freq = T, xlab = "Distance between villages (km)", main = NULL)


#calculation of the coefficient of variation (k) and the average cluster person-year (csize)----

k<-sd(randomizationTable[["ipa.2022"]])/mean(randomizationTable[["ipa.2022"]])
csize<-ceiling(mean(randomizationTable[["Population size"]]))
hcsize<-ceiling(hmean(randomizationTable[["Population size"]]))

#csize<-200

#other variables required to calculate the number of required clusters to detect a difference in incidence (Hayes/Bennett 1999)
lambda0<-mean(randomizationTable[["ipa.2022"]])
zalpha<-1.96
zbeta<-0.8
reduction<-c(0.1,
             0.125,
             0.25,
             0.375,
             0.5,
             0.6,
             0.65,
             0.7,
             0.75,
             0.8,
             0.85,
             0.9,
             0.95)


# calculation of the Number of required clusters ----

Hayes_Bennett1999<-function(lambda0=NULL,
                            k=NULL,
                            csize=NULL,
                            hcsize=NULL,
                            reduction=NULL,
                            zalpha=1.96,
                            zbeta=0.8
                            ){
  data1<-data.frame(lambda0=NULL,
                    reduction=NULL,
                    lambda1=NULL,
                    zalpha=NULL,
                    zbeta=NULL,
                    k=NULL,
                    csize=NULL,
                    hcsize=NULL,
                    nclust=NULL,
                    arm.size=NULL,
                    total.size=NULL,
                    OR=NULL)
  
  for (n in reduction){
    data0<-as.data.frame(lambda0)
    data0$reduction<-n
    data0$lambda1<-lambda0*(1-n)
    data0$zalpha<-zalpha
    data0$zbeta<-zbeta
    data0$k<-k
    data0$csize<-csize
    data0$hcsize<-hcsize
    data0$nclust<- ceiling(1+((zalpha+zbeta)^2)*(((lambda0+data0$lambda1)/csize)+k^2*(lambda0^2+data0$lambda1^2))/(lambda0-data0$lambda1)^2)
    data0$arm.size<-data0$nclust*csize
    data0$total.size<-data0$arm.size*2
    data0$OR<-1/((lambda0/(1-lambda0))/(data0$lambda1/(1-data0$lambda1)))
    data1<-rbind(data1,data0)
  }
  return(data1)
}


nclusterTable<-Hayes_Bennett1999(lambda0 = lambda0,k = k,
                  csize = csize,
                  hcsize = hcsize,
                  reduction = reduction)


nclust = 16


zbeta = c(.8, .85, .9)

reduction_table = NULL

for(beta in zbeta){
  
  n = 1000
  reduction = .1
  
  while(n > nclust){
    
    temp<-Hayes_Bennett1999(lambda0 = lambda0,
                            
                            k = k,
                            csize = csize,
                            hcsize = hcsize,
                            reduction = reduction,
                            zbeta=beta)
    
    n = temp$nclust
    reduction = reduction + .001
    
  }
  
  reduction_table = rbind(reduction_table, data.frame(reduction = reduction, beta = beta))
  
}





write.csv(nclusterTable,"nclusterTable_250.csv")

write.csv(randomizationTable, 'randomizationTable.csv', quote = F, row.names = F)


############Step 1 ----
#Genrate all possible permutations of arms allocation respecting no sellected (0), control (1) and intervention (2)

arm0 <- c(rep(0,5),rep(1,14),rep(2,14))
set.seed(1)
arm0permutations<-as.data.frame(t(replicate(1440000,sample(arm0,length(arm0),replace = F))))
arm0permutationsUnique <- arm0permutations[!duplicated(arm0permutations),]
write.csv(arm0permutationsUnique, 'arm0permutationsUnique.csv', quote = F, row.names = F)


## Pre selected

coords <- randomizationTable[, c("Latitude", "Longitude")]   # coordinates
map_data   <- randomizationTable[,c('Village',
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


## No selected 

coords <- no_selected_highapi[, c("Latitude", "Longitude")]   # coordinates
map_data   <- no_selected_highapi[,c('Village',
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

communities_no_selected <- SpatialPointsDataFrame(coords = coords,
                                                  data = map_data, 
                                                  proj4string = crs)



p2 <- tm_shape(communities_preselected)+
  tm_dots("2022", size = 0.08, style="pretty", col = "lightblue")+
  tm_shape(communities_no_selected)+
  tm_dots("2022", size = 0.08, style="pretty", col = "red")+
  # tm_shape(communities_NoSelected)+
  #tm_dots(col="lightblue", size = 0.08)+
  tm_basemap('OpenStreetMap')+
  tm_shape(rivers_sp)+
  tm_text("River", size = 1.15)+
  tm_shape(communities_number_sp)+
  tm_text("order", size=0.6)+
  tm_scale_bar() 


p2

min(randomizationTable$`2022`)
