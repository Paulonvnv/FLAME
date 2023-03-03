library(geosphere)

setwd("/Users/sfine/Desktop/FLAME")
randomizationTable <- read.csv("RandomizationTable_22FEB2023.csv")

# function of the harmonic mean and its standard deviation
hmean<-function(x) {1/mean(1/x)}
sd.hmean<-function(x) {sqrt((mean(1/x))^(-4)*var(1/x)/length(x))}

#LINES 14 AND 15 OF THIS CODE ARE NOT WORKING
#imputation of population size with the harmonic mean
#randomizationTable[is.na(randomizationTable$`Population.size`),][["Population.size"]]<-ceiling(hmean(randomizationTable[!is.na(randomizationTable$`Population.size`),][["Population.size"]]))


#calculation of the coefficient of variation (k) and the average cluster person-year (csize)----

k<-sd(randomizationTable[["ipa.2022"]])/mean(randomizationTable[["ipa.2022"]])
csize<-ceiling(mean(randomizationTable[["Population.size"]]))
hcsize<-ceiling(hmean(randomizationTable[["Population.size"]]))

hist(randomizationTable$`Population.size`,breaks = 50, freq = T, xlab = "Population size", main = NULL)

#new code for michelle
log_pop_size = log(randomizationTable$Population.size)
randomizationTable$log_pop_size = log_pop_size
hist(randomizationTable$`log_pop_size`,breaks = 50, freq = T, xlab = "Log Population Size", main = NULL, xlim=c(0,8))


hist(randomizationTable$`X2022`,breaks = 50, freq = T, xlab = "Malaria cases 2022", main = NULL, xlim=c(0,100))


#other variables required to calculate the number of required clusters to detect a difference in incidence (Hayes/Bennett 1999)
lambda0<-mean(randomizationTable[["ipa.2022"]])
zalpha<-1.96
zbeta<-0.85 #changing this to 0.85 from 0.80
reduction<-c(0.5,
             0.6,
             0.65,
             0.7,
             0.75,
             0.8,
             0.85)


# calculation of the Number of required clusters ----

Hayes_Bennett1999<-function(lambda0=NULL,
                            k=NULL,
                            csize=NULL,
                            hcsize=NULL,
                            reduction=NULL,
                            zalpha=1.96,
                            zbeta=0.85
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


write.csv(nclusterTable,"nclusterTable_alleligiblecomm_nocommelim_0.85power.csv")

#eliminate the top 2 largest communities
randomizationTable_wolargecomm <- read.csv("RandomizationTable_wolargecomm_22FEB2023.csv")


# function of the harmonic mean and its standard deviation
hmean<-function(x) {1/mean(1/x)}
sd.hmean<-function(x) {sqrt((mean(1/x))^(-4)*var(1/x)/length(x))}

#LINES 14 AND 15 OF THIS CODE ARE NOT WORKING
#imputation of population size with the harmonic mean
#randomizationTable[is.na(randomizationTable$`Population.size`),][["Population.size"]]<-ceiling(hmean(randomizationTable[!is.na(randomizationTable$`Population.size`),][["Population.size"]]))


#calculation of the coefficient of variation (k) and the average cluster person-year (csize)----

k<-sd(randomizationTable_wolargecomm[["ipa.2022"]])/mean(randomizationTable_wolargecomm[["ipa.2022"]])
csize<-ceiling(mean(randomizationTable_wolargecomm[["Population.size"]]))
hcsize<-ceiling(hmean(randomizationTable_wolargecomm[["Population.size"]]))

hist(randomizationTable$`Population.size`,breaks = 50, freq = T, xlab = "Population size", main = NULL)


#other variables required to calculate the number of required clusters to detect a difference in incidence (Hayes/Bennett 1999)
lambda0<-mean(randomizationTable_wolargecomm[["ipa.2022"]])
zalpha<-1.96
zbeta<-0.9 #changing this
reduction<-c(0.5,
             0.6,
             0.65,
             0.7,
             0.75,
             0.8,
             0.85)


# calculation of the Number of required clusters ----

Hayes_Bennett1999<-function(lambda0=NULL,
                            k=NULL,
                            csize=NULL,
                            hcsize=NULL,
                            reduction=NULL,
                            zalpha=1.96,
                            zbeta=0.9
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


nclusterTable2<-Hayes_Bennett1999(lambda0 = lambda0,k = k,
                                 csize = csize,
                                 hcsize = hcsize,
                                 reduction = reduction)


write.csv(nclusterTable,"nclusterTable_wolarge2communities_0.90power.csv")



####Eliminate the top 2 largest communities and 1 smallest community
randomizationTable_wolargewotinycomm <- read.csv("RandomizationTable_wolarge2wotinycomm_22FEB2023.csv")


# function of the harmonic mean and its standard deviation
hmean<-function(x) {1/mean(1/x)}
sd.hmean<-function(x) {sqrt((mean(1/x))^(-4)*var(1/x)/length(x))}

#LINES 14 AND 15 OF THIS CODE ARE NOT WORKING
#imputation of population size with the harmonic mean
#randomizationTable[is.na(randomizationTable$`Population.size`),][["Population.size"]]<-ceiling(hmean(randomizationTable[!is.na(randomizationTable$`Population.size`),][["Population.size"]]))


#calculation of the coefficient of variation (k) and the average cluster person-year (csize)----

k<-sd(randomizationTable_wolargewotinycomm[["ipa.2022"]])/mean(randomizationTable_wolargewotinycomm[["ipa.2022"]])
csize<-ceiling(mean(randomizationTable_wolargewotinycomm[["Population.size"]]))
hcsize<-ceiling(hmean(randomizationTable_wolargewotinycomm[["Population.size"]]))



#other variables required to calculate the number of required clusters to detect a difference in incidence (Hayes/Bennett 1999)
lambda0<-mean(randomizationTable_wolargewotinycomm[["ipa.2022"]])
zalpha<-1.96
zbeta<-0.9 #changing this
reduction<-c(0.5,
             0.6,
             0.65,
             0.7,
             0.75,
             0.8,
             0.85)


# calculation of the Number of required clusters ----

Hayes_Bennett1999<-function(lambda0=NULL,
                            k=NULL,
                            csize=NULL,
                            hcsize=NULL,
                            reduction=NULL,
                            zalpha=1.96,
                            zbeta=0.9
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


nclusterTable3<-Hayes_Bennett1999(lambda0 = lambda0,k = k,
                                  csize = csize,
                                  hcsize = hcsize,
                                  reduction = reduction)


write.csv(nclusterTable,"nclusterTable_wolarge2wosmallest1comm_0.9power.csv")


