#########
# This script calculate scaled-age in 2015. PesudoAge at flowering is 85. 
#########
source("scripts/colname2rgb.R")
# Caluculate flowring age
flowringDateList = read.table("Os2015/Os2015_input/2015CSSLs_headingDate.txt", header = T, stringsAsFactors = F)
flowringDateList[flowringDateList=="-"] = "1990/1/1"
tmp = matrix(unlist(apply(flowringDateList,c(1,2),FUN = function(x){strsplit(x,"/")})), byrow = T, ncol = 3)
tmp = apply(tmp,1, FUN = function(x){sprintf("%d-%02d-%02d",as.numeric(x[1]),as.numeric(x[2]),as.numeric(x[3]))})
flowringDate = matrix(tmp, byrow = F, ncol = ncol(flowringDateList))

SeedSowingDateList <- c("2015-04-16", "2015-04-30", "2015-05-14", "2015-05-28")
TransPlantDateList <- c("2015-05-14", "2015-05-21", "2015-06-04", "2015-06-18")
SeedSowingDate <- c(rep(SeedSowingDateList[1],5), rep(SeedSowingDateList[2],5), rep(SeedSowingDateList[3],5), rep(SeedSowingDateList[4],5))
SeedSowingDate = matrix(rep(SeedSowingDate,nrow(flowringDate)), byrow = T, ncol = length(SeedSowingDate))
flowringAge = matrix(as.Date(flowringDate) - as.Date(SeedSowingDate), byrow = F, ncol = ncol(flowringDate))
flowringAge[flowringAge<0] = NA #Remove dummy (1990/1/1)
colnames(flowringAge) = colnames(flowringDateList)
rownames(flowringAge) = rownames(flowringDateList)

# Caluculate average flowering age
sowingSet = list(1:5,6:10,11:15,16:20)
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


Unpredictable = rep(NA, 4)
flowringAge.ave = rbind(flowringAge.ave,Unpredictable)

# Interporation of flowringAge.ave
for(i in 1:nrow(flowringAge.ave)){
  tmp = flowringAge.ave[i,]
  if(sum(is.na(tmp))>0 & sum(is.na(tmp))<3){
    sp = spline(colnames(flowringAge.ave),tmp,n = as.numeric(colnames(flowringAge.ave)[4])-as.numeric(colnames(flowringAge.ave)[1])+1)
    tmp = sp$y[is.element(sp$x, as.numeric(colnames(flowringAge.ave)))]
  } else {
    tmp[is.na(tmp)] = 120
  }
  flowringAge.ave[i,] = tmp
}

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
