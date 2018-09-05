#########
# This script calculate scaled age in 2016. PesudoAge at flowering is 85. 
#########
source("scripts/colname2rgb.R")
# Caluculate flowring age
flowringDateList = read.table("Os2016/Os2016_input/2016CSSLs_headingDate.txt", header = T, stringsAsFactors = F)
flowringDateList = cbind(flowringDateList[,1:5],flowringDateList[,1:5], flowringDateList[,6:10],flowringDateList[,6:10])
flowringDateList[flowringDateList=="-"] = "1990/1/1"
tmp = matrix(unlist(apply(flowringDateList,c(1,2),FUN = function(x){strsplit(x,"/")})), byrow = T, ncol = 3)
tmp = apply(tmp,1, FUN = function(x){sprintf("%d-%02d-%02d",as.numeric(x[1]),as.numeric(x[2]),as.numeric(x[3]))})
flowringDate = matrix(tmp, byrow = F, ncol = ncol(flowringDateList))

SeedSowingDateList <- c("2016-04-21","2016-05-19")
TransPlantDateList <- c("2016-05-16","2016-06-08")
SeedSowingDate <- c(rep(SeedSowingDateList[1],5), rep(SeedSowingDateList[2],5))
SeedSowingDate = matrix(rep(SeedSowingDate,nrow(flowringDate)), byrow = T, ncol = length(SeedSowingDate))
flowringAge = matrix(as.Date(flowringDate) - as.Date(SeedSowingDate), byrow = F, ncol = ncol(flowringDate))
flowringAge[flowringAge<0] = NA #Remove dummy (1990/1/1)
colnames(flowringAge) = colnames(flowringDateList)
rownames(flowringAge) = rownames(flowringDateList)

# Caluculate average flowering age
sowingSet = list(1:5,6:10)
flowringAge.ave = matrix(NA, ncol = length(sowingSet), nrow = nrow(flowringDate))
rownames(flowringAge.ave) = rownames(flowringDateList)
colnames(flowringAge.ave) = as.Date(SeedSowingDateList)
i = 1
for(group in sowingSet){
  flowringAge.ave[,i] = apply(flowringAge[,group],1,FUN = function(x){
    x = x[!is.na(x)]
    if(length(x)>0){
      return(mean(x))
    } else {
      return(NA)  
    }
  }
  )
  i = i + 1
}


Unpredictable = rep(NA, 2)
flowringAge.ave = rbind(flowringAge.ave,Unpredictable)

######### Caluculate pseudoAge
pesudoAge = function(y){
  x = y[1]
  line = y[2]
  TransPlantSet = as.numeric(y[3])
  sp = SeedSowingDateList[TransPlantSet]
  Age = as.numeric(as.Date(x)-as.Date(sp))
  flowringAge = flowringAge.ave[line,TransPlantSet]
  if(Age <= flowringAge){
    pesudoAge = 85/flowringAge*Age
  } else {
    pesudoAge = 85+ Age - flowringAge
  }
  return(round(pesudoAge))
}

