#library(xlsx)

rm(list=c("combns",
          "csize",
          "dists",
          "Hayes_Bennett1999",
          "hcsize",
          "hdists",
          "k",
          "lambda0",
          "nclusterTable",
          "reduction",
          "zalpha",
          "zbeta"))

   

library(combinat)


#Subset to actual randomization variables
randomizationTable <- randomizationTable[,c("village",
                                            "district",
                                            "basin",
                                            "order",
                                            "selected",
                                            "dist_iquitos",
                                            "pviv2018",
                                            "pviv2019",
                                            "population",
                                            "api.pviv2018",
                                            "api.pviv2019",
                                            "dist_minutes_cat3",
                                            "dist_minutes_cat2",
                                            "dist_minutes_cat1",
                                            "pop2k",
                                            "pop5k",
                                            "access",
                                            "XGD",
                                            "YGD"
)]


# data imputation----

randomizationTable[is.na(randomizationTable$dist_minutes_cat1),][["dist_minutes_cat1"]]<-hmean(randomizationTable[randomizationTable$dist_minutes_cat1!=0,][["dist_minutes_cat1"]])
randomizationTable[is.na(randomizationTable$access),][["access"]]<-hmean(randomizationTable[!is.na(randomizationTable$access),][["access"]])

############Step 1 ----
#Genrate all possible permutations of arms allocation respecting no sellected (0), control (1) and intervention (2)

arm0 <- c(rep(0,7),rep(1,16),rep(2,16))
sample(1:100000,1)
set.seed(1)
arm0permutations<-as.data.frame(t(replicate(1440000,sample(arm0,length(arm0),replace = F))))
arm0permutations1<-arm0permutations[1:360000,]
arm0permutations2<-arm0permutations[360001:720000,]
arm0permutations3<-arm0permutations[720001:1080000,]
arm0permutations4<-arm0permutations[1080001:1440000,]

arm0permutations<-arm0permutations1

rm(list=c("arm0permutations1",
          "arm0permutations2",
          "arm0permutations3",
          "arm0permutations4"))

arm0permutationsUnique <- arm0permutations[!duplicated(arm0permutations),]
rownames(arm0permutationsUnique) <- NULL
keepRandomization.df <- matrix(data=NA, nrow=nrow(arm0permutationsUnique),1)



############Step 2 ----
#sd.population<-sd(randomizationTable$population,na.rm = T)       #a
sd.dist_iquitos<-sd(randomizationTable$dist_iquitos,na.rm = T)   #b
#sd.dis_cat1<-sd(randomizationTable$dist_minutes_cat1,na.rm = T)  #c
#sd.malaria2018<-sd(randomizationTable$malaria2018,na.rm = T)    #d
#sd.malaria2019<-sd(randomizationTable$malaria2019,na.rm = T)     #e
#sd.api.malaria2018<-sd(randomizationTable$api.malaria2018,na.rm = T)    #f
sd.api.pviv2019<-sd(randomizationTable$api.pviv2019,na.rm = T)     #g
sd.popk2<-sd(randomizationTable$pop2k,na.rm = T)                #h
#sd.access<-sd(randomizationTable$access,na.rm = T)              #i
minDistBetweenArms<-2000                                         #j

# Step 3: Randomization, strategy 1 ----
# Balance between control and intervention
# Distance between villages that belongs to different arms no included

for (i in 840001:870000){ #3 seconds
  name <- paste("randomizationArm",i)
  randomizationArm <- t(as.data.frame(arm0permutationsUnique[i,]))
  randomizationTable[,c(name)] <- randomizationArm
    
  ############Step 3
  #calculate the mean and SD overall and by Arm 1/Arm 2
  
  #a. population
  
  # balance between 0, 1 and 2
  # diffInMean.a <- c(abs(by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[3])
  #                   )
  # 
  # isDiffInMean.a.LowerThanOverallSD <- prod(diffInMean.a < 0.25 * sd.population)
  
  # balance just between 1 and 2
  # diffInMean.a <- abs(by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[3])
  # 
  # isDiffInMean.a.LowerThanOverallSD <- diffInMean.a < 0.25 * sd.population

  #b. distance from the village to Iquitos city in meters
  
  # balance between 0, 1 and 2
  # diffInMean.b <- c(abs(by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[3])
  #                   )
  # 
  # isDiffInMean.b.LowerThanOverallSD <- prod(diffInMean.b < 0.25 * sd.dist_iquitos)
   
  diffInMean.b <- abs(by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[3])

  isDiffInMean.b.LowerThanOverallSD <- diffInMean.b < 0.25 * sd.dist_iquitos
  
  #c. distance to a health center cat1
  
  # balance between 0, 1 and 2
  # diffInMean.c <- c(abs(by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[3])
  #                   )
  # 
  # isDiffInMean.c.LowerThanOverallSD <- prod(diffInMean.c < 0.25 * sd.dis_cat1)
  
  # diffInMean.c <- abs(by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[3])
  # 
  # isDiffInMean.c.LowerThanOverallSD <- diffInMean.c < 0.25 * sd.dis_cat1
  
    
  #d. total malaria cases in the village in 2018
  
  # balance between 0, 1 and 2
  # diffInMean.d <- c(abs(by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[3])
  #                   )
  # 
  # isDiffInMean.d.LowerThanOverallSD <- prod(diffInMean.d < 0.25 * sd.malaria2018)
  
  # diffInMean.d <- abs(by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[3])
  # 
  # isDiffInMean.d.LowerThanOverallSD <- diffInMean.d < 0.25 * sd.malaria2018
  
  #e. total malaria cases in the village in 2019
  
  # balance between 0, 1 and 2
  # diffInMean.e <- c(abs(by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[3])
  #                   )
  # 
  # isDiffInMean.e.LowerThanOverallSD <- prod(diffInMean.e < 0.25 * sd.malaria2019)
  
  # diffInMean.e <- abs(by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[3])
  # 
  # isDiffInMean.e.LowerThanOverallSD <- diffInMean.e < 0.25 * sd.malaria2019

  
  
  
  #f. total malaria api in the village in 2018
  
  # balance between 0, 1 and 2
  # diffInMean.f <- c(abs(by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[3])
  #                   )
  # 
  # isDiffInMean.f.LowerThanOverallSD <- prod(diffInMean.f < 0.25 * sd.malaria2018)
  
  # diffInMean.f <- abs(by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[3])
  # 
  # isDiffInMean.f.LowerThanOverallSD <- diffInMean.f < 0.25 * sd.malaria2018
  
  #g. total malaria api in the village in 2019
  
  # balance between 0, 1 and 2
  diffInMean.g <- c(abs(by(randomizationTable$api.pviv2019,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$api.pviv2019,randomizationTable[,c(name)],mean, na.rm=T)[2]),
                    abs(by(randomizationTable$api.pviv2019,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$api.pviv2019,randomizationTable[,c(name)],mean, na.rm=T)[3]),
                    abs(by(randomizationTable$api.pviv2019,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$api.pviv2019,randomizationTable[,c(name)],mean, na.rm=T)[3])
  )

  isDiffInMean.g.LowerThanOverallSD <- prod(diffInMean.g < 0.25 * sd.api.pviv2019)
  
  # diffInMean.g <- abs(by(randomizationTable$api.malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$api.malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[3])
  # 
  # isDiffInMean.g.LowerThanOverallSD <- diffInMean.g < 0.25 * sd.api.malaria2019
  # 
  
  
  #h. population density estimated from worldpop
  
  # balance between 0, 1 and 2
  # diffInMean.h <- c(abs(by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[3])
  # )
  # 
  # isDiffInMean.h.LowerThanOverallSD <- prod(diffInMean.h < 0.25 * sd.popk2)
  
  diffInMean.h <- abs(by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[3])

  isDiffInMean.h.LowerThanOverallSD <- diffInMean.h < 0.25 * sd.popk2
  
  
  #i. accees to the closer river
  
  # balance between 0, 1 and 2
  # diffInMean.i <- c(abs(by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[3])
  #                   )
  # 
  # isDiffInMean.i.LowerThanOverallSD <- prod(diffInMean.i < 0.25 * sd.malaria2019)
    
  # diffInMean.i <- abs(by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[3])
  # 
  # isDiffInMean.i.LowerThanOverallSD <- diffInMean.i < 0.25 * sd.access
  
  
  #j. distance between villages that belongs to different arms higher that threshold (2000 meters)
  
  # distance between arm1 & arm2, and arm2 & arm0 higher than threshold
  isDist.HigherThan.minDistBetweenArms.a<-prod(distm(randomizationTable[randomizationTable[,c(name)]==1,c("XGD","YGD")],
                                                     randomizationTable[randomizationTable[,c(name)]==2,c("XGD","YGD")],
                                                     fun = distGeo)>minDistBetweenArms)
  # isDist.HigherThan.minDistBetweenArms.b<-prod(distm(randomizationTable[randomizationTable[,c(name)]==1,c("XGD","YGD")],
  #                                                    randomizationTable[randomizationTable[,c(name)]==0,c("XGD","YGD")],
  #                                                    fun = distGeo)>minDistBetweenArms)
  isDist.HigherThan.minDistBetweenArms.c<-prod(distm(randomizationTable[randomizationTable[,c(name)]==2,c("XGD","YGD")],
                                                     randomizationTable[randomizationTable[,c(name)]==0,c("XGD","YGD")],
                                                     fun = distGeo)>minDistBetweenArms)

  isDist.HigherThan.minDistBetweenArms<-prod(c(isDist.HigherThan.minDistBetweenArms.a,
                                               #isDist.HigherThan.minDistBetweenArms.b,
                                               isDist.HigherThan.minDistBetweenArms.c))

  # just distance between arm1 & arm2 higher than threshold
  # isDist.HigherThan.minDistBetweenArms<-prod(distm(randomizationTable[randomizationTable[,c(name)]==1,c("XGD","YGD")], randomizationTable[randomizationTable[,c(name)]==2,c("XGD","YGD")], fun = distGeo)>minDistBetweenArms)
    
    ############Step 4
    #Keep the potential randomization if the difference in the means for Arm 1 and Arm 2 for a, b, and c is less than 25% of the overall SD for the given variable.
    #Repeat until all potential randomizations are listed 
    keepRandomization.df[i,1] <- prod(c(#isDiffInMean.a.LowerThanOverallSD,
                                        isDiffInMean.b.LowerThanOverallSD,
                                        #isDiffInMean.c.LowerThanOverallSD,
                                        #isDiffInMean.d.LowerThanOverallSD,
                                        #isDiffInMean.e.LowerThanOverallSD,
                                        #isDiffInMean.f.LowerThanOverallSD,
                                        isDiffInMean.g.LowerThanOverallSD,
                                        isDiffInMean.h.LowerThanOverallSD,
                                        #isDiffInMean.i.LowerThanOverallSD,
                                        isDist.HigherThan.minDistBetweenArms
                                        ),
                                      na.rm = T)
  }


# Step 3: Randomization, strategy 2 ----
# Balance between control and intervention
# Distance between villages that belongs to different arms included

for (i in 90001: 120000){ #3 seconds
  name <- paste("randomizationArm",i)
  randomizationArm <- t(as.data.frame(arm0permutationsUnique[i,]))
  randomizationTable[,c(name)] <- randomizationArm
  
  ############Step 3
  #calculate the mean and SD overall and by Arm 1/Arm 2
  
  #a. population
  
  # balance between 0, 1 and 2
  # diffInMean.a <- c(abs(by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[3])
  #                   )
  # 
  # isDiffInMean.a.LowerThanOverallSD <- prod(diffInMean.a < 0.25 * sd.population)
  
  # balance just between 1 and 2
  diffInMean.a <- abs(by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[3])
  
  isDiffInMean.a.LowerThanOverallSD <- diffInMean.a < 0.25 * sd.population
  
  #b. distance from the village to Iquitos city in meters
  
  # balance between 0, 1 and 2
  # diffInMean.b <- c(abs(by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[3])
  #                   )
  # 
  # isDiffInMean.b.LowerThanOverallSD <- prod(diffInMean.b < 0.25 * sd.dist_iquitos)
  
  diffInMean.b <- abs(by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[3])
  
  isDiffInMean.b.LowerThanOverallSD <- diffInMean.b < 0.25 * sd.dist_iquitos
  
  #c. distance to a health center cat1
  
  # balance between 0, 1 and 2
  # diffInMean.c <- c(abs(by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[3])
  #                   )
  # 
  # isDiffInMean.c.LowerThanOverallSD <- prod(diffInMean.c < 0.25 * sd.dis_cat1)
  
  diffInMean.c <- abs(by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[3])
  
  isDiffInMean.c.LowerThanOverallSD <- diffInMean.c < 0.25 * sd.dis_cat1
  
  
  #d. total malaria cases in the village in 2018
  
  # balance between 0, 1 and 2
  # diffInMean.d <- c(abs(by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[3])
  #                   )
  # 
  # isDiffInMean.d.LowerThanOverallSD <- prod(diffInMean.d < 0.25 * sd.malaria2018)
  
  # diffInMean.d <- abs(by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[3])
  # 
  # isDiffInMean.d.LowerThanOverallSD <- diffInMean.d < 0.25 * sd.malaria2018
  
  #e. total malaria cases in the village in 2019
  
  # balance between 0, 1 and 2
  # diffInMean.e <- c(abs(by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[3])
  #                   )
  # 
  # isDiffInMean.e.LowerThanOverallSD <- prod(diffInMean.e < 0.25 * sd.malaria2019)
  
  diffInMean.e <- abs(by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[3])
  
  isDiffInMean.e.LowerThanOverallSD <- diffInMean.e < 0.25 * sd.malaria2019
  
  
  
  
  #f. total malaria api in the village in 2018
  
  # balance between 0, 1 and 2
  # diffInMean.f <- c(abs(by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[3])
  #                   )
  # 
  # isDiffInMean.f.LowerThanOverallSD <- prod(diffInMean.f < 0.25 * sd.malaria2018)
  
  # diffInMean.f <- abs(by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[3])
  # 
  # isDiffInMean.f.LowerThanOverallSD <- diffInMean.f < 0.25 * sd.malaria2018
  
  #g. total malaria api in the village in 2019
  
  # balance between 0, 1 and 2
  diffInMean.g <- c(abs(by(randomizationTable$api.malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$api.malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[2]),
                    abs(by(randomizationTable$api.malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$api.malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[3]),
                    abs(by(randomizationTable$api.malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$api.malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[3])
  )
  
  isDiffInMean.g.LowerThanOverallSD <- prod(diffInMean.g < 0.25 * sd.api.malaria2019)
  
  # diffInMean.g <- abs(by(randomizationTable$api.malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$api.malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[3])
  # 
  # isDiffInMean.g.LowerThanOverallSD <- diffInMean.g < 0.25 * sd.api.malaria2019
  # 
  
  
  #h. population density estimated from worldpop
  
  # balance between 0, 1 and 2
  # diffInMean.h <- c(abs(by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[3])
  # )
  # 
  # isDiffInMean.h.LowerThanOverallSD <- prod(diffInMean.h < 0.25 * sd.popk2)
  
  diffInMean.h <- abs(by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[3])
  
  isDiffInMean.h.LowerThanOverallSD <- diffInMean.h < 0.25 * sd.popk2
  
  
  #i. accees to the closer river
  
  # balance between 0, 1 and 2
  # diffInMean.i <- c(abs(by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[3])
  #                   )
  # 
  # isDiffInMean.i.LowerThanOverallSD <- prod(diffInMean.i < 0.25 * sd.malaria2019)
  
  # diffInMean.i <- abs(by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[3])
  # 
  # isDiffInMean.i.LowerThanOverallSD <- diffInMean.i < 0.25 * sd.access
  
  
  #j. distance between villages that belongs to different arms higher that threshold (2000 meters)
  
  # distance between arm1 & arm2, and arm2 & arm0 higher than threshold
  isDist.HigherThan.minDistBetweenArms.a<-prod(distm(randomizationTable[randomizationTable[,c(name)]==1,c("XGD","YGD")],
                                                     randomizationTable[randomizationTable[,c(name)]==2,c("XGD","YGD")],
                                                     fun = distGeo)>minDistBetweenArms)
  # isDist.HigherThan.minDistBetweenArms.b<-prod(distm(randomizationTable[randomizationTable[,c(name)]==1,c("XGD","YGD")],
  #                                                    randomizationTable[randomizationTable[,c(name)]==0,c("XGD","YGD")],
  #                                                    fun = distGeo)>minDistBetweenArms)
  isDist.HigherThan.minDistBetweenArms.c<-prod(distm(randomizationTable[randomizationTable[,c(name)]==2,c("XGD","YGD")],
                                                     randomizationTable[randomizationTable[,c(name)]==0,c("XGD","YGD")],
                                                     fun = distGeo)>minDistBetweenArms)
  
  isDist.HigherThan.minDistBetweenArms<-prod(c(isDist.HigherThan.minDistBetweenArms.a,
                                               #isDist.HigherThan.minDistBetweenArms.b,
                                               isDist.HigherThan.minDistBetweenArms.c))
  
  # just distance between arm1 & arm2 higher than threshold
  # isDist.HigherThan.minDistBetweenArms<-prod(distm(randomizationTable[randomizationTable[,c(name)]==1,c("XGD","YGD")], randomizationTable[randomizationTable[,c(name)]==2,c("XGD","YGD")], fun = distGeo)>minDistBetweenArms)
  
  ############Step 4
  #Keep the potential randomization if the difference in the means for Arm 1 and Arm 2 for a, b, and c is less than 25% of the overall SD for the given variable.
  #Repeat until all potential randomizations are listed 
  keepRandomization.df[i,1] <- prod(c(isDiffInMean.a.LowerThanOverallSD,
                                      isDiffInMean.b.LowerThanOverallSD,
                                      isDiffInMean.c.LowerThanOverallSD,
                                      #isDiffInMean.d.LowerThanOverallSD,
                                      isDiffInMean.e.LowerThanOverallSD,
                                      #isDiffInMean.f.LowerThanOverallSD,
                                      isDiffInMean.g.LowerThanOverallSD,
                                      isDiffInMean.h.LowerThanOverallSD,
                                      #isDiffInMean.i.LowerThanOverallSD,
                                      isDist.HigherThan.minDistBetweenArms
  ),
  na.rm = T)
}


# Step 3: Randomization ----

for (i in 1: nrow(arm0permutationsUnique)){ #3 seconds
  name <- paste("randomizationArm",i)
  randomizationArm <- t(as.data.frame(arm0permutationsUnique[i,]))
  randomizationTable[,c(name)] <- randomizationArm
  
  ############Step 3
  #calculate the mean and SD overall and by Arm 1/Arm 2
  
  #a. population
  
  # balance between 0, 1 and 2
  # diffInMean.a <- c(abs(by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[3])
  #                   )
  # 
  # isDiffInMean.a.LowerThanOverallSD <- prod(diffInMean.a < 0.25 * sd.population)
  
  # balance just between 1 and 2
  diffInMean.a <- abs(by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$population,randomizationTable[,c(name)],mean, na.rm=T)[3])
  
  isDiffInMean.a.LowerThanOverallSD <- diffInMean.a < 0.25 * sd.population
  
  #b. distance from the village to Iquitos city in meters
  
  # balance between 0, 1 and 2
  # diffInMean.b <- c(abs(by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[3])
  #                   )
  # 
  # isDiffInMean.b.LowerThanOverallSD <- prod(diffInMean.b < 0.25 * sd.dist_iquitos)
  
  diffInMean.b <- abs(by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$dist_iquitos,randomizationTable[,c(name)],mean, na.rm=T)[3])
  
  isDiffInMean.b.LowerThanOverallSD <- diffInMean.b < 0.25 * sd.dist_iquitos
  
  #c. distance to a health center cat1
  
  # balance between 0, 1 and 2
  # diffInMean.c <- c(abs(by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[3])
  #                   )
  # 
  # isDiffInMean.c.LowerThanOverallSD <- prod(diffInMean.c < 0.25 * sd.dis_cat1)
  
  diffInMean.c <- abs(by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[3])
  
  isDiffInMean.c.LowerThanOverallSD <- diffInMean.c < 0.25 * sd.dis_cat1
  
  
  #d. total malaria cases in the village in 2018
  
  # balance between 0, 1 and 2
  # diffInMean.d <- c(abs(by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[3])
  #                   )
  # 
  # isDiffInMean.d.LowerThanOverallSD <- prod(diffInMean.d < 0.25 * sd.malaria2018)
  
  # diffInMean.d <- abs(by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[3])
  # 
  # isDiffInMean.d.LowerThanOverallSD <- diffInMean.d < 0.25 * sd.malaria2018
  
  #e. total malaria cases in the village in 2019
  
  # balance between 0, 1 and 2
  # diffInMean.e <- c(abs(by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[3])
  #                   )
  # 
  # isDiffInMean.e.LowerThanOverallSD <- prod(diffInMean.e < 0.25 * sd.malaria2019)
  
  diffInMean.e <- abs(by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[3])
  
  isDiffInMean.e.LowerThanOverallSD <- diffInMean.e < 0.25 * sd.malaria2019
  
  
  
  
  #f. total malaria api in the village in 2018
  
  # balance between 0, 1 and 2
  # diffInMean.f <- c(abs(by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[3])
  #                   )
  # 
  # isDiffInMean.f.LowerThanOverallSD <- prod(diffInMean.f < 0.25 * sd.malaria2018)
  
  # diffInMean.f <- abs(by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$api.malaria2018,randomizationTable[,c(name)],mean, na.rm=T)[3])
  # 
  # isDiffInMean.f.LowerThanOverallSD <- diffInMean.f < 0.25 * sd.malaria2018
  
  #g. total malaria api in the village in 2019
  
  # balance between 0, 1 and 2
  diffInMean.g <- c(abs(by(randomizationTable$api.malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$api.malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[2]),
                    abs(by(randomizationTable$api.malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$api.malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[3]),
                    abs(by(randomizationTable$api.malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$api.malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[3])
  )
  
  isDiffInMean.g.LowerThanOverallSD <- prod(diffInMean.g < 0.25 * sd.api.malaria2019)
  
  # diffInMean.g <- abs(by(randomizationTable$api.malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$api.malaria2019,randomizationTable[,c(name)],mean, na.rm=T)[3])
  # 
  # isDiffInMean.g.LowerThanOverallSD <- diffInMean.g < 0.25 * sd.api.malaria2019
  # 
  
  
  #h. population density estimated from worldpop
  
  # balance between 0, 1 and 2
  # diffInMean.h <- c(abs(by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[3])
  # )
  # 
  # isDiffInMean.h.LowerThanOverallSD <- prod(diffInMean.h < 0.25 * sd.popk2)
  
  diffInMean.h <- abs(by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$pop2k,randomizationTable[,c(name)],mean, na.rm=T)[3])
  
  isDiffInMean.h.LowerThanOverallSD <- diffInMean.h < 0.25 * sd.popk2
  
  
  #i. accees to the closer river
  
  # balance between 0, 1 and 2
  # diffInMean.i <- c(abs(by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[2]),
  #                   abs(by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[1]-by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[3]),
  #                   abs(by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[3])
  #                   )
  # 
  # isDiffInMean.i.LowerThanOverallSD <- prod(diffInMean.i < 0.25 * sd.malaria2019)
  
  # diffInMean.i <- abs(by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$access,randomizationTable[,c(name)],mean, na.rm=T)[3])
  # 
  # isDiffInMean.i.LowerThanOverallSD <- diffInMean.i < 0.25 * sd.access
  
  
  #j. distance between villages that belongs to different arms higher that threshold (2000 meters)
  
  # distance between arm1 & arm2, and arm2 & arm0 higher than threshold
  isDist.HigherThan.minDistBetweenArms.a<-prod(distm(randomizationTable[randomizationTable[,c(name)]==1,c("XGD","YGD")],
                                                     randomizationTable[randomizationTable[,c(name)]==2,c("XGD","YGD")],
                                                     fun = distGeo)>minDistBetweenArms)
  # isDist.HigherThan.minDistBetweenArms.b<-prod(distm(randomizationTable[randomizationTable[,c(name)]==1,c("XGD","YGD")],
  #                                                    randomizationTable[randomizationTable[,c(name)]==0,c("XGD","YGD")],
  #                                                    fun = distGeo)>minDistBetweenArms)
  isDist.HigherThan.minDistBetweenArms.c<-prod(distm(randomizationTable[randomizationTable[,c(name)]==2,c("XGD","YGD")],
                                                     randomizationTable[randomizationTable[,c(name)]==0,c("XGD","YGD")],
                                                     fun = distGeo)>minDistBetweenArms)
  
  isDist.HigherThan.minDistBetweenArms<-prod(c(isDist.HigherThan.minDistBetweenArms.a,
                                               #isDist.HigherThan.minDistBetweenArms.b,
                                               isDist.HigherThan.minDistBetweenArms.c))
  
  # just distance between arm1 & arm2 higher than threshold
  # isDist.HigherThan.minDistBetweenArms<-prod(distm(randomizationTable[randomizationTable[,c(name)]==1,c("XGD","YGD")], randomizationTable[randomizationTable[,c(name)]==2,c("XGD","YGD")], fun = distGeo)>minDistBetweenArms)
  
  ############Step 4
  #Keep the potential randomization if the difference in the means for Arm 1 and Arm 2 for a, b, and c is less than 25% of the overall SD for the given variable.
  #Repeat until all potential randomizations are listed 
  keepRandomization.df[i,1] <- prod(c(isDiffInMean.a.LowerThanOverallSD,
                                      isDiffInMean.b.LowerThanOverallSD,
                                      isDiffInMean.c.LowerThanOverallSD,
                                      #isDiffInMean.d.LowerThanOverallSD,
                                      isDiffInMean.e.LowerThanOverallSD,
                                      #isDiffInMean.f.LowerThanOverallSD,
                                      isDiffInMean.g.LowerThanOverallSD,
                                      isDiffInMean.h.LowerThanOverallSD,
                                      #isDiffInMean.i.LowerThanOverallSD,
                                      isDist.HigherThan.minDistBetweenArms
  ),
  na.rm = T)
}







############Step 7 ----
#Randomly select the potential allocations meeting the criteria.
listOfIndex <- which(keepRandomization.df[]==1,arr.ind = T)
listOf_validRandomizationArm <- paste("randomizationArm",listOfIndex[,1])

# n valid randomization out m (number of sampled permutations)
rtab <- randomizationTable[,c("village",
                              "district",
                              "basin",
                              "order",
                              "selected",
                              "dist_iquitos",
                              "pviv2018",
                              "pviv2019",
                              "population",
                              "api.pviv2018",
                              "api.pviv2019",
                              "dist_minutes_cat3",
                              "dist_minutes_cat2",
                              "dist_minutes_cat1",
                              "pop2k",
                              "pop5k",
                              "access","XGD","YGD", listOf_validRandomizationArm)]

randomizationTable<-rtab


setwd("D:/UTSW/fMDA_Project/Randomization/")


write.table(rtab,"random3/finalRandomizedTable3.csv", sep = ",", quote = F,row.names = F)
write.table(randomizationTable,"random3/RandomizedTable3.csv", sep = ",", quote = F,row.names = F)
write.table(keepRandomization.df,"random3/keepRandomization.df.csv", sep = ",", quote = F,row.names = F)


# Step 8: Map ----

#select the number and which valid randomization arms to plot

possibleRandomizations<-names(rtab[-1:-19])

n<-sample(1:length(possibleRandomizations),6)

n<-1:2


rtabs_to_plot<-rtab[,c("village",
        "district",
        "basin",
        "order",
        "selected",
        "dist_iquitos",
        "malaria2018",
        "malaria2019",
        "population",
        "api.malaria2018",
        "api.malaria2019",
        "dist_minutes_cat3",
        "dist_minutes_cat2",
        "dist_minutes_cat1",
        "pop2k",
        "pop5k",
        "access","XGD","YGD",possibleRandomizations[n])]

library(raster)
library(ggplot2); library(dplyr);library(leaflet);library(sp); library(tmap); library(tmaptools); library(mapview)
#ubicaicon de las comunidades

setwd("D:/UTSW/fMDA_Project/Previous_Data/Cases2013-2019/mapa_prueba/")

for (i in names(rtabs_to_plot[-1:-19])){
  
  communities<-rtabs_to_plot[,c("village",
                                "district",
                                "basin",
                                "order",
                                "selected",
                                "dist_iquitos",
                                "malaria2018",
                                "malaria2019",
                                "population",
                                "api.malaria2018",
                                "api.malaria2019",
                                "dist_minutes_cat3",
                                "dist_minutes_cat2",
                                "dist_minutes_cat1",
                                "pop2k",
                                "pop5k",
                                "access","XGD", "YGD",i)]
  
  
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
  
  coords <- communities[communities[i]==1, c("XGD", "YGD")]   # coordinates
  data   <- communities[communities[i]==1,c("village",
                                            "district",
                                            "basin",
                                            "order",
                                            "selected",
                                            "dist_iquitos",
                                            "malaria2018",
                                            "malaria2019",
                                            "population",
                                            "api.malaria2018",
                                            "api.malaria2019",
                                            "dist_minutes_cat3",
                                            "dist_minutes_cat2",
                                            "dist_minutes_cat1",
                                            "pop2k",
                                            "pop5k",
                                            "access","XGD", "YGD")]          # data
  
  crs    <- CRS("+init=epsg:4326") # proj4string of coords
  
  communities_controlArm <- SpatialPointsDataFrame(coords = coords,
                                                   data = data, 
                                                   proj4string = crs)
  
  
  # Intervention Arm
  
  coords <- communities[communities[i]==2, c("XGD", "YGD")]   # coordinates
  data   <- communities[communities[i]==2,c("village",
                                            "district",
                                            "basin",
                                            "order",
                                            "selected",
                                            "dist_iquitos",
                                            "malaria2018",
                                            "malaria2019",
                                            "population",
                                            "api.malaria2018",
                                            "api.malaria2019",
                                            "dist_minutes_cat3",
                                            "dist_minutes_cat2",
                                            "dist_minutes_cat1",
                                            "pop2k",
                                            "pop5k",
                                            "access","XGD", "YGD")]          # data
  
  crs    <- CRS("+init=epsg:4326") # proj4string of coords
  
  communities_interventionArm <- SpatialPointsDataFrame(coords = coords,
                                                        data = data, 
                                                        proj4string = crs)
  
  
  
  ## No selected
  
  coords <- communities[communities[i]==0, c("XGD", "YGD")]   # coordinates
  data   <- communities[communities[i]==0,c("village",
                                            "district",
                                            "basin",
                                            "order",
                                            "selected",
                                            "dist_iquitos",
                                            "malaria2018",
                                            "malaria2019",
                                            "population",
                                            "api.malaria2018",
                                            "api.malaria2019",
                                            "dist_minutes_cat3",
                                            "dist_minutes_cat2",
                                            "dist_minutes_cat1",
                                            "pop2k",
                                            "pop5k",
                                            "access","XGD", "YGD")]          # data
  
  crs    <- CRS("+init=epsg:4326") # proj4string of coords
  
  communities_NoSelected <- SpatialPointsDataFrame(coords = coords,
                                                   data = data, 
                                                   proj4string = crs)
  
  
  ## numero de comunidad
  
  names(communities)
  coords <- communities[ , c("XGD", "YGD")]   # coordinates
  data   <- communities[,c("order","XGD", "YGD")]          # data
  crs    <- CRS("+init=epsg:4326") # proj4string of coords
  
  communities_number_sp <- SpatialPointsDataFrame(coords = coords,
                                                  data = data, 
                                                  proj4string = crs)
  
  ##rios
  coords <- rivers[ , c("long", "lat")]   # coordinates
  data   <- rivers          # data
  crs    <- CRS("+init=epsg:4326") # proj4string of coords
  
  rivers_sp <- SpatialPointsDataFrame(coords = coords,
                                      data = data, 
                                      proj4string = crs)
  
  ##iquitos
  coords <- Iquitos[ , c("long", "lat")]   # coordinates
  data   <- Iquitos          # data
  crs    <- CRS("+init=epsg:4326") # proj4string of coords
  Iquitos_sp <- SpatialPointsDataFrame(coords = coords,
                                       data = data, 
                                       proj4string = crs)
  ##iquitos texto
  coords <- Iquitos_text[ , c("long", "lat")]   # coordinates
  data   <- Iquitos_text          # data
  crs    <- CRS("+init=epsg:4326") # proj4string of coords
  Iquitos_text_sp <- SpatialPointsDataFrame(coords = coords,
                                            data = data, 
                                            proj4string = crs)
  
  #geometria de loreto
  loreto <- shapefile("Limite_departamental/BAS_LIM_DEPARTAMENTO.shp")
  loreto <- loreto[loreto@data$NOMBDEP == "LORETO",]
  
  
  ### Generacion de mapas
  tmap_mode('view') #pone tmap en modo exploración
  # mapa de las comunidades
  
  p1 <- tm_shape(communities_controlArm)+
    tm_dots("api.malaria2019", size = 0.08, style="pretty",col = "green")+
    tm_shape(communities_interventionArm)+
    tm_dots("api.malaria2019", size = 0.08, style="pretty", col = "red")+
    tm_shape(communities_NoSelected)+
    tm_dots(col="lightblue", size = 0.08)+
    tm_basemap('OpenStreetMap')+
    tm_shape(rivers_sp)+
    tm_text("River", size = 1.15)+
    tm_shape(communities_number_sp)+
    tm_text("order", size=0.6)+
    tm_scale_bar()  
  
  assign(paste("map",i,sep = "_"),p1)
  
}


`map_randomizationArm 15494`
`map_randomizationArm 18389`
`map_randomizationArm 28275`
`map_randomizationArm 6990`
`map_randomizationArm 7334`
`map_randomizationArm 9747`

`map_randomizationArm 17577`
`map_randomizationArm 39385`

map_randomizationArm.39385


#Randomly pick one arm----
#set.seed(2)
#save(".Random.seed",file="random_seed1.RData")


rtab_r<-sample(names(rtab[-1:-18]),1)

finalRandomizedTable <- rtab[,c("village",
                                "district",
                                "basin",
                                "order",
                                "selected",
                                "dist_iquitos",
                                "malaria2018",
                                "malaria2019",
                                "population",
                                "api.malaria2018",
                                "api.malaria2019",
                                "dist_minutes_cat3",
                                "dist_minutes_cat2",
                                "dist_minutes_cat1",
                                "pop2k",
                                "pop5k",
                                "access","XGD","YGD",finalrandomization)]

#randomization Dec 7 2017
write.table(finalRandomizedTable,"finalRandomizedTable.csv", sep = ",", quote = F)


#----

# data("World")# extrae geometrias del mundo
# mapa general de ubicacion
# p2 <- tm_shape(World[World$name=='Peru',])+tm_polygons() + tm_shape(loreto)+tm_polygons(border.col = "red")+
#   tm_scale_bar() + tm_shape(Iquitos_sp) + tm_dots(col = "red") + tm_shape(Iquitos_text_sp)+tm_text("Community", size = 1.25)
# # pone ambos mapas en paralelo
# map <- tmap_arrange(p1,p2,ncol=2)









#ESS: effective sample size
#m: number of subjects in a cluster
#k: number of clusters
#rho: intracluster correlation coefficient
#DE: Design Effect

DE<-1-rho(m-1)

ESS<- m*k/DE
