
w = 4
n = 48

randomizationTable = read.csv('randomizationTable.csv')

names(randomizationTable) = c("village_district",
                              "Village",
                              "District",
                              "Province",
                              "Latitude",
                              "Longitude",
                              "Population size",
                              "dist_minutes_cat1",
                              "dist_minutes_cat3",
                              "2021",
                              "2022",
                              "order",
                              "ipa.2021",
                              "ipa.2022")

arm0permutationsUnique = read.csv('arm11permutationsUnique.csv')

s = round(seq(1,nrow(arm0permutationsUnique)+1, length.out=n+1))
low = s[w]
high = s[w+1]-1

keepRandomization.df <- matrix(data=NA, nrow= high - low + 1, 1)

############Step 2 ----
sd.population<-sd(randomizationTable$`Population size`,na.rm = T)       #a
sd.dis_cat3<-sd(randomizationTable$dist_minutes_cat3,na.rm = T)  #b
sd.dis_cat1<-sd(randomizationTable$dist_minutes_cat1,na.rm = T)  #c
sd.ipa.2022<-sd(randomizationTable$ipa.2022,na.rm = T)     #d
minDistBetweenArms<-2000   

# Step 3: Randomization ----
# Balance between control and intervention
# Distance between villages that belongs to different arms no included

for (i in low:high){ #3 seconds
  name <- paste("randomizationArm",i)
  randomizationArm <- t(as.data.frame(arm0permutationsUnique[i,]))
  randomizationTable[,c(name)] <- randomizationArm
  
  ############Step 3
  #calculate the mean and SD overall and by Arm 1/Arm 2
  
  # a. population
  # balance just between 1 and 2
  diffInMean.a <- abs(by(randomizationTable$`Population size`,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$`Population size`,randomizationTable[,c(name)],mean, na.rm=T)[3])
  
  isDiffInMean.a.LowerThanOverallSD <- diffInMean.a < 0.25 * sd.population
  
  #b. distance from the village to Iquitos city in minutes
  
  diffInMean.b <- abs(by(randomizationTable$dist_minutes_cat3,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$dist_minutes_cat3,randomizationTable[,c(name)],mean, na.rm=T)[3])
  
  isDiffInMean.b.LowerThanOverallSD <- diffInMean.b < 0.25 * sd.dis_cat3
  
  #c. distance to a health center cat1 in minutes
  
  diffInMean.c <- abs(by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$dist_minutes_cat1,randomizationTable[,c(name)],mean, na.rm=T)[3])
  
  isDiffInMean.c.LowerThanOverallSD <- diffInMean.c < 0.25 * sd.dis_cat1
  
  #g. total malaria api in the village in 2022
  
  diffInMean.d <- abs(by(randomizationTable$ipa.2022,randomizationTable[,c(name)],mean, na.rm=T)[2]-by(randomizationTable$ipa.2022,randomizationTable[,c(name)],mean, na.rm=T)[3])
  
  isDiffInMean.d.LowerThanOverallSD <- diffInMean.d < 0.25 * sd.ipa.2022
  
  
  #e. distance between villages that belongs to different arms higher that threshold (2000 meters)
  
  # just distance between arm1 & arm2 higher than threshold
  isDist.HigherThan.minDistBetweenArms <-prod(distm(randomizationTable[randomizationTable[,c(name)]==1,c("Longitude","Latitude")], randomizationTable[randomizationTable[,c(name)]==2,c("Longitude","Latitude")], fun = distGeo)>minDistBetweenArms)
  
  ############Step 4
  #Keep the potential randomization if the difference in the means for Arm 1 and Arm 2 for a, b, and c is less than 25% of the overall SD for the given variable.
  #Repeat until all potential randomizations are listed 
  keepRandomization.df[i - low + 1,1] <- prod(c(isDiffInMean.a.LowerThanOverallSD,
                                      isDiffInMean.b.LowerThanOverallSD,
                                      isDiffInMean.c.LowerThanOverallSD,
                                      isDiffInMean.d.LowerThanOverallSD,
                                      isDist.HigherThan.minDistBetweenArms),
  na.rm = T)
}

############Step 7 ----
#Randomly select the potential allocations meeting the criteria.
listOfIndex <- which(keepRandomization.df[]==1,arr.ind = T)
listOf_validRandomizationArm <- paste("randomizationArm", listOfIndex[,1] + low - 1)

randomizationTable <- randomizationTable[,c("village_district",
                              "Village",
                              "District",
                              "Province",
                              "Latitude",
                              "Longitude",
                              "Population size",
                              "dist_minutes_cat1",
                              "dist_minutes_cat3",
                              "2021",
                              "2022",
                              "order",
                              "ipa.2021",
                              "ipa.2022" , listOf_validRandomizationArm)]

write.csv(randomizationTable, paste0('randomizationTable11_',w,'.csv'), row.names = FALSE)

listOf_validRandomizationArm


