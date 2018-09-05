################Prepare meteorological data
load("Os2015/Os2015_input/Hirakata_10min_2015")
wt.10 = as.data.frame(wt)
sp = approxfun(seq(10,nrow(wt.10)*10 ,10)[!is.na(wt.10$`air temperature`)],wt.10$`air temperature`[!is.na(wt.10$`air temperature`)],rule = 2)

load("Os2015/Os2015_input/wtdata_2015_Osaka")
wt.1 = wt
tmp = sp(seq(1,nrow(wt.1)))
wt.1$temperature = tmp
tt <- with(wt.1, sprintf("%s-%02d-%02d %02d:%02d:00", year, month, day, hour,min))
st <- strptime(tt, "%Y-%m-%d %H:%M:%S")
st.ori <- strptime("2015-01-01 00:00:00", "%Y-%m-%d %H:%M:%S")
time <- as.numeric(st)/60 - as.numeric(st.ori)/60 +1
wt = cbind(time,wt.1)
wt = wt[order(wt$time),]
save(wt, file = "Os2015/wtdata_2015_Takatsuki")

load("Os2016/Os2016_input/Kyotanabe_10min_2016")
wt.10 = as.data.frame(wt)
sp = approxfun(seq(10,nrow(wt.10)*10 ,10)[!is.na(wt.10$`air temperature`)],wt.10$`air temperature`[!is.na(wt.10$`air temperature`)],rule = 2)

load("Os2016/Os2016_input/wtdata_2016_Nara")
wt.1 = wt
tmp = sp(seq(1,nrow(wt.1)))
wt.1$temperature = tmp
tt <- with(wt.1, sprintf("%s-%02d-%02d %02d:%02d:00", year, month, day, hour,min))
st <- strptime(tt, "%Y-%m-%d %H:%M:%S")
st.ori <- strptime("2016-01-01 00:00:00", "%Y-%m-%d %H:%M:%S")
time <- as.numeric(st)/60 - as.numeric(st.ori)/60 +1
wt = cbind(time,wt.1)
wt = wt[order(wt$time),]
save(wt, file = "Os2016/wtdata_2016_Kizugawa")

#############Caluculate correlation of records in Aug. between by Observation machines in the fields and of JMA
######2015
wt.record.2015 = read.csv("Os2015/Os2015_input/2015_02-2015_08_wt_record_field.csv", header = T)
load("Os2015/wtdata_2015_Takatsuki")
# Parse Aug. data in field
wt.record.2015.Aug = wt.record.2015[grep("8/",wt.record.2015$X),]

# Parse Aug. data of JMA
wt.Aug = wt[wt$year==2015&wt$month==8,]
# Caluculate day average
days = unique(wt.Aug$day)
wt.Aug.ave = NULL
for(d in days){
  wt.Aug.ave = rbind(wt.Aug.ave,apply(wt.Aug[wt.Aug$day==d,c("day","temperature","radiation")],2,mean))
}
#Caluculate correlation
cor.temp.2015 = cor(wt.record.2015.Aug$temp, wt.Aug.ave[,2])
cor.rad.2015 = cor(wt.record.2015.Aug$rad, wt.Aug.ave[,3])
cat(nrow(wt.Aug.ave))

######2016
wt.record.2016 = read.csv("Os2016/Os2016_input/wt_records_field_2016kizu.csv", header = T)
load("Os2016/wtdata_2016_Kizugawa")
# Caluculate day average
days = unique(wt.record.2016$date)
wt.record.2016.Aug.ave = NULL
for(d in days){
  tmp = wt.record.2016[wt.record.2016$date==d,c("date","temp","raddiation")]
  for(i in 1:(nrow(tmp)/60)){
    wt.record.2016.Aug.ave = rbind(wt.record.2016.Aug.ave,apply(tmp[seq(60*(i-1)+1,60),],2,FUN = function(x){return(mean(as.numeric(x)))}))
  }
}

# Parse Aug. data of JMA
wt.Aug = wt[wt$year==2016&wt$month==8,]
# Caluculate day average
days = unique(wt.Aug$day)
wt.Aug.ave = NULL
for(d in days){
  tmp = wt.Aug[wt.Aug$day==d,c("day","temperature","radiation")]
  for(i in 1:(nrow(tmp)/60)){
    wt.Aug.ave = rbind(wt.Aug.ave,apply(tmp[seq(60*(i-1)+1,60),],2,FUN = function(x){return(mean(as.numeric(x)))}))
  }
}
#Caluculate correlation
cor.temp.2016 = cor(wt.record.2016$temp, wt.Aug$temperature)
cor.rad.2016 = cor(wt.record.2016$raddiation, wt.Aug$radiation)
cat(nrow(wt.Aug))


################## Interpolate missing weather data for FIT
# 2015
load("Os2015/Os2015_input/Hirakata_10min_2015")
wt.10 = as.data.frame(wt)
sp = approxfun(seq(10,nrow(wt.10)*10 ,10)[!is.na(wt.10$`air temperature`)],wt.10$`air temperature`[!is.na(wt.10$`air temperature`)],rule = 2)
load("Os2015/Os2015_input/wtdata_2015_Osaka")
wt.1 = wt
tmp = sp(seq(1,nrow(wt.1)))
wt.1$temperature = tmp
tt <- with(wt.1, sprintf("%s-%02d-%02d %02d:%02d:00", year, month, day, hour,min))
st <- strptime(tt, "%Y-%m-%d %H:%M:%S")
st.ori <- strptime("2015-01-01 00:00:00", "%Y-%m-%d %H:%M:%S")
time <- as.numeric(st)/60 - as.numeric(st.ori)/60 +1
wt = cbind(time,wt.1)
wt = wt[order(wt$time),]
save(wt, file = "Os2015/Os2015_FIT_input/wtdata_2015_Takatsuki")

#2016
load("Os2016/Os2016_input/Kyotanabe_10min_2016")
wt.10 = as.data.frame(wt)
sp = approxfun(seq(10,nrow(wt.10)*10 ,10)[!is.na(wt.10$`air temperature`)],wt.10$`air temperature`[!is.na(wt.10$`air temperature`)],rule = 2)
load("Os2016/Os2016_input/wtdata_2016_Nara")
wt.1 = wt
tmp = sp(seq(1,nrow(wt.1)))
wt.1$temperature = tmp
tt <- with(wt.1, sprintf("%s-%02d-%02d %02d:%02d:00", year, month, day, hour,min))
st <- strptime(tt, "%Y-%m-%d %H:%M:%S")
st.ori <- strptime("2016-01-01 00:00:00", "%Y-%m-%d %H:%M:%S")
time <- as.numeric(st)/60 - as.numeric(st.ori)/60 +1
wt = cbind(time,wt.1)
wt = wt[order(wt$time),]
save(wt, file = "Os2016/Os2016_FIT_input/wtdata_2016_Kizugawa")

################### Prepar Dark/Light condition list 
###########Parameters##############
load("Os2015/Os2015_FIT_input/wtdata_2015_Takatsuki")
load("Os2015/Sampling_timepoints_2015")
wt.time = sprintf("%04d-%02d-%02d-%02d-%02d", wt$year, wt$month, wt$day, wt$hour, wt$min)
rownames(wt) = wt.time
radiation = wt[sprintf("%s-00", names(timepoints)),]$radiation
dark = radiation<0.3 
sp = 0
ep = 0
is.dark = FALSE
dark.time = list()
for(i in 1:length(dark)){
  if(dark[i]==TRUE && !is.dark){
    is.dark = TRUE
    sp = i
  } else if(dark[i] == FALSE && is.dark){
    is.dark = FALSE
    ep = i - 1
    dark.time = c(dark.time, list(c(sp, ep)))
  }
}
if(is.dark){
  dark.time = c(dark.time, list(c(sp, i)))
}
DarkLight = list()
for(i in 1:length(dark.time)){
  dark = dark.time[[i]]
  sp = dark[1]
  ep = dark[2]
  DarkLight = c(DarkLight,list(c(sp,ep,ep,sp)))
}
save(DarkLight, file = "Os2015/DarkLight_Takatsuki_2015")

############### Fig. 1c
library(lubridate)
# set parameters -----------------------------------------------------
#load sample attribute 
at = read.table("Os2015/Os2015_input/SampleAttribute_2015_Takatsuki.txt", header = T, stringsAsFactors = F, fill = T )

#Parse timepoints
timepoints = unique(sprintf("%s:%s:%s:%s:0", at$year, at$month, at$day, at$hour))

# Load weather data
load("Os2015/Os2015_FIT_input/wtdata_2015_Takatsuki")
wt = wt[((wt$min==0) & (is.element(wt$month, c(5,6,7,8,9)))),]
time.sum = unique(sprintf("%s:%s:%s:%s:%s", wt$year, wt$month, wt$day, wt$hour, wt$min))
names(time.sum) = seq(1,length(time.sum))

#Caluculate month border and sampling time 
table.accum = function(x){
  tmp = table(x)
  for(i in 2:length(tmp)){
    tmp[i] = tmp[i-1] + tmp[i]
  }
  return(tmp)
}
month.border = c(0,table.accum(wt$month)+0.5)
sampling.time = names(time.sum)[is.element(time.sum, timepoints)]

# Make list of samplin day set
tmp = strsplit(unique(sprintf("%s:%s",at$month, at$day)),":")
sampling.day.set = list()
for(i in 1:(length(tmp)/2)){
  a = tmp[[2*i-1]]
  b = tmp[[2*i]]
  sampling.day.set = c(sampling.day.set, list(c(a,b[2])))
}

pdf("Figures/fig1c.pdf", width = 19)
#Parse wt data of successive 2 days
wt2 = wt[((is.element(wt$min, seq(0,50,10))) & (is.element(wt$month, c(sampling.day.set[[i]][1]))) & (is.element(wt$day,sampling.day.set[[i]][2:3]))),]
time.sum2 = unique(sprintf("%s:%s:%s:%s:%s", wt2$year, wt2$month, wt2$day, wt2$hour, wt2$min))
names(time.sum2) = seq(1,length(time.sum2))
sampling.time2 = names(time.sum2)[is.element(time.sum2, timepoints)]

layout(matrix(c(1,1,1,3,2,2,2,4), ncol = 4, byrow = T))
plot(wt$temperature, type = "n", main = "Temperature")
lines(wt$temperature)
abline(v = month.border, lty = "dashed", col = "gray")
abline(v = sampling.time, col = rgb(1,0,0,0.1))

plot(wt$radiation, type = "n", main = "Radiation")
lines(wt$radiation)
abline(v = month.border, lty = "dashed", col = "gray")
abline(v = sampling.time, col = rgb(1,0,0,0.1))

dev.off()



################### Prepare rawcnt matrix for the 2015 dataset
# set parameters -----------------------------------------------------
dir.rawdata <- "Os2015/Os2015_input/rsem_out_2015/"

#load library attribute --------------
at <- read.delim("Os2015/Os2015_input/Sample-IndexPrimer.txt", header=T, as.is=T)
rownames(at) <- sprintf("%s_%03d", at[,"library"], at[,"index"])

#load transcript description --------------------
load("General_input/141103-1_GeneDescription_Osa")

#set data read row --------------
drr <- des[,"NormalizationGroup"]=="data"
names(drr) <- rownames(des)


# save/load expression data table-------------------------------------
rawcnt <- matrix(0, nrow=nrow(des), ncol=nrow(at))
colnames(rawcnt) <- rownames(at)
rownames(rawcnt) <- rownames(des)
readcount <- matrix(0, nrow=nrow(at), ncol=4)
colnames(readcount) <- c("Total reads", "Unique sequences", "Mapped reads", "Mapped rate")

for(i in 1:nrow(at)){
  fn <- sprintf("%s/%s/Index%03d.genes.results",
                dir.rawdata, at[i,"library"], at[i,"index"])
  
  tmp <- read.delim(fn, nrows=5, fill=T, header=F, as.is=T)
  readcount[i,] <- c(tmp[2,2], tmp[3,2], tmp[4,2], tmp[5,2])
  #readcount[i,] <- c(tmp[2,2], tmp[2,2], tmp[3,2], tmp[4,2])
  
  tmp <- read.delim(fn, as.is=T, comment.char="#")
  rawcnt[,i] <- tmp[,"expected_count"]
  
  cat(i)
  cat("\n")
}

# subtruction of indexing contamination
ulib <- unique(at[,"library"])

rawcnttmp <- NULL
for(i in ulib){
  extmp <- rawcnt[, at[,"library"]==i]
  tmp <- rowSums(extmp)*0.0005
  extmp <- extmp-tmp
  rawcnttmp <- cbind(rawcnttmp, extmp)
}
rawcnttmp[rawcnttmp<0] <- 0
rawcnt <- rawcnttmp[,rownames(at)]
rawseq = rawcnt

# marge count data from 1 sample to 1 coloumn
uni.sampleID <- unique(at[,"sampleID"])

# at
at.tmp <- NULL
for(i in 1:length(uni.sampleID)){
  tmp <- at[at[,"sampleID"] == uni.sampleID[i], ]
  
  if(is.vector(tmp)){
    at.tmp <- rbind(at.tmp, tmp)
  } else {
    at.tmp <- rbind(at.tmp, tmp[1,])
  }
}
at.ori <- at

# Marge rawcnt
tmp <- rawcnt
tmp.fun <- function(x){
  x2 <- aggregate(x, by=list(at[,"sampleID"]), FUN=sum)
  x3 <- x2[,2]
  names(x3) <- x2[,1]
  return(x3)
}
tmp2 <- apply(tmp, 1, FUN=tmp.fun)
tmp3 <- t(tmp2)
rawcnt <- tmp3

#calculate rpm
rce <- rawcnt[des[,"NormalizationGroup"]=="data", ]
tmp <- colSums(rce)/10^6
rce2 <- t(t(rce)/tmp)
if(sum(des[,"NormalizationGroup"]=="data")==0){rce2<-NULL}

rpm <- rce2
rpm[is.nan(rpm)] <- 0

# save objects
rawcnt.original <- rawcnt
rpm.original <- rpm
species <- "Osa_2015"
fn.rawcnt <- sprintf("Os2015/rawcnt-original_%s", species)
fn.rpm <- sprintf("Os2015/rpm-original_%s", species)
save(rawcnt.original, file=fn.rawcnt)
save(rpm.original, file=fn.rpm)

################### Prepare timepoints list of sampling in 2015
at = read.table("Os2015/Os2015_input/SampleAttribute_2015_Takatsuki.txt", header = T, stringsAsFactors = F, fill = T )
rownames(at) = at[,1]
timepoints = sort(unique(sprintf("%04d-%02d-%02d-%02d", at$year, at$month, at$day, at$hour)))
tmp = seq(1, length(timepoints))
names(tmp) = timepoints
timepoints = tmp
save(timepoints, file = "Os2015/Sampling_timepoints_2015")

###################Prepare rawcnt matrix for the 2015 dataset
# set parameters -----------------------------------------------------
dir.rawdata <- "Os2015/Os2015_input/rsem_out_2015/"

#load library attribute --------------
at <- read.delim("Os2015/Os2015_input/Sample-IndexPrimer.txt", header=T, as.is=T)
rownames(at) <- sprintf("%s_%03d", at[,"library"], at[,"index"])

#load transcript description --------------------
load("General_input/141103-1_GeneDescription_Osa")

#set data read row --------------
drr <- des[,"NormalizationGroup"]=="data"
names(drr) <- rownames(des)

# save/load expression data table-------------------------------------
rawcnt <- matrix(0, nrow=nrow(des), ncol=nrow(at))
colnames(rawcnt) <- rownames(at)
rownames(rawcnt) <- rownames(des)
readcount <- matrix(0, nrow=nrow(at), ncol=4)
colnames(readcount) <- c("Total reads", "Unique sequences", "Mapped reads", "Mapped rate")

for(i in 1:nrow(at)){
  fn <- sprintf("%s/%s/Index%03d.genes.results",
                dir.rawdata, at[i,"library"], at[i,"index"])
  
  tmp <- read.delim(fn, nrows=5, fill=T, header=F, as.is=T)
  readcount[i,] <- c(tmp[2,2], tmp[3,2], tmp[4,2], tmp[5,2])
  tmp <- read.delim(fn, as.is=T, comment.char="#")
  rawcnt[,i] <- tmp[,"expected_count"]
  
  cat(i)
  cat("\n")
}

# subtruction of indexing contamination
ulib <- unique(at[,"library"])

rawcnttmp <- NULL
for(i in ulib){
  extmp <- rawcnt[, at[,"library"]==i]
  tmp <- rowSums(extmp)*0.0005
  extmp <- extmp-tmp
  rawcnttmp <- cbind(rawcnttmp, extmp)
}
rawcnttmp[rawcnttmp<0] <- 0
rawcnt <- rawcnttmp[,rownames(at)]

# marge count data from 1 sample to 1 coloumn
uni.sampleID <- unique(at[,"sampleID"])

# at
at.tmp <- NULL
for(i in 1:length(uni.sampleID)){
  tmp <- at[at[,"sampleID"] == uni.sampleID[i], ]
  
  if(is.vector(tmp)){
    at.tmp <- rbind(at.tmp, tmp)
  } else {
    at.tmp <- rbind(at.tmp, tmp[1,])
  }
}
at.ori <- at

# Marge rawcnt
tmp <- rawcnt
tmp.fun <- function(x){
  x2 <- aggregate(x, by=list(at[,"sampleID"]), FUN=sum)
  x3 <- x2[,2]
  names(x3) <- x2[,1]
  return(x3)
}
tmp2 <- apply(tmp, 1, FUN=tmp.fun)
tmp3 <- t(tmp2)
rawcnt <- tmp3

# save objects
rawcnt.original <- rawcnt
species <- "Osa_2015"
fn.rawcnt <- sprintf("Os2015/rawcnt-original_%s", species)
save(rawcnt.original, file=fn.rawcnt)


pdf("Figures/supfig1a.pdf")
hist(sort(log10(colSums(rawcnt.original[des[rownames(rawcnt.original[,is.element(colnames(rawcnt.original),at$sampleID)]),"NormalizationGroup"]=="data",]))),col = "grey")
abline(v = 5, col = "red")
dev.off()
rawcnt.original = rawcnt.original[,is.element(colnames(rawcnt.original), at$sampleID)]

# Filtering samples by raw read count
rawcnt <- rawcnt.original[des[rownames(rawcnt.original),"NormalizationGroup"]=="data",]
rawcnt = rawcnt[,log10(colSums(rawcnt))>=5] #Remove samples with less than 10^5 reads

pdf("Figures/supfig1b.pdf")
hist(rowSums(rawcnt>0), col = "grey", ylim = c(0,10000), xlim = c(0,1000))
abline(v = ncol(rawcnt)*0.2, col = "red")
dev.off()

# Filtering samples and genes by raw read count
at = read.table("Os2015/Os2015_input/SampleAttribute_2015_Takatsuki.txt", header = T, stringsAsFactors = F, fill = T )
rownames(at) = at[,1]
at <- at[is.element(rownames(at),colnames(rawcnt)),]
rawcnt <- rawcnt[rowSums(rawcnt>0)>ncol(rawcnt)*0.2,] # Substruct genes expressed in more than 20% samples
save(rawcnt, file = "Os2015/rawcnt-filtered_Osa_2015")

###################Prepare rawcnt matrix for the 2016 dataset
# set parameters -----------------------------------------------------
dir.rawdata <- "Os2016/Os2016_input/rsem_out_2016"

#load library attribute --------------
at <- read.delim("Os2016/Os2016_input/Sample-IndexPrimer.txt", header=T, as.is=T)
rownames(at) <- sprintf("%s_%03d", at[,"library"], at[,"index"])

#load transcript description --------------------
load("General_input/141103-1_GeneDescription_Osa")

#set data read row --------------
drr <- des[,"NormalizationGroup"]=="data"
names(drr) <- rownames(des)

# save/load expression data table-------------------------------------
rawcnt <- matrix(0, nrow=nrow(des), ncol=nrow(at))
colnames(rawcnt) <- at$sampleID
rownames(rawcnt) <- rownames(des)
readcount <- matrix(0, nrow=nrow(at), ncol=4)
colnames(readcount) <- c("Total reads", "Unique sequences", "Mapped reads", "Mapped rate")

for(i in 1:nrow(at)){
  fn <- sprintf("%s/%s/final/Index%03d.genes.results",
                dir.rawdata, at[i,"library"], at[i,"index"])
  
  tmp <- read.delim(fn, nrows=5, fill=T, header=F, as.is=T)
  readcount[i,] <- c(tmp[2,2], tmp[3,2], tmp[4,2], tmp[5,2])
  tmp <- read.delim(fn, as.is=T, comment.char="#")
  rawcnt[,i] <- tmp[,"expected_count"]
  
  cat(i)
  cat("\n")
}

# subtruction of indexing contamination
ulib <- unique(at[,"library"])

rawcnttmp <- NULL
for(i in ulib){
  extmp <- rawcnt[, at[,"library"]==i]
  tmp <- rowSums(extmp)*0.0005
  extmp <- extmp-tmp
  rawcnttmp <- cbind(rawcnttmp, extmp)
}
rawcnttmp[rawcnttmp<0] <- 0
rawcnt <- rawcnttmp[,as.character(at$sampleID)]
colnames(rawcnt) = as.numeric(colnames(rawcnt))

# save objects
at = read.table("Os2016/Os2016_input/SampleAttribute_2016_Kizu.txt", header = T, stringsAsFactors = F, fill = T )
rownames(at) = as.numeric(at[,1])
rawcnt.original <- rawcnt[,is.element(colnames(rawcnt),rownames(at))]
species <- "Osa_2016"
fn.rawcnt <- sprintf("Os2016/rawcnt-original_%s", species)
save(rawcnt.original, file=fn.rawcnt)

# Filtering samples by raw read count
rawcnt <- rawcnt.original[des[rownames(rawcnt.original),"NormalizationGroup"]=="data",]
rawcnt = rawcnt[,log10(colSums(rawcnt))>=5] #Remove samples with less than 10^5 reads
rawcnt.2016 = rawcnt

# Extruct 2015-passed-genes 
load("Os2015/rawcnt-filtered_Osa_2015")
rawcnt = rawcnt.2016[rownames(rawcnt),]
save(rawcnt, file = "Os2016/rawcnt-filtered_Osa_2016")

################### Prepare timepoints list of sampling in 2016
at = read.table("Os2016/Os2016_input/SampleAttribute_2016_Kizu.txt", header = T, stringsAsFactors = F, fill = T )
rownames(at) = at[,1]
timepoints = sort(unique(sprintf("%04d-%02d-%02d-%02d", at$year, at$month, at$day, at$hour)))
tmp = seq(1, length(timepoints))
names(tmp) = timepoints
timepoints = tmp
save(timepoints, file = "Os2016/Sampling_timepoints_2016")


################### Fig. 1d
################
### function ###
################
library(fields)
source("scripts/ng.Colors.R")
source("scripts/ng.BHFDR.R")

#set range
ng.mkrg <- function(input, mg.ratio=0.1){
  rg <- max(input)-min(input)
  mg <- rg*mg.ratio
  out <- c(min(input)-mg, max(input)+mg)
  return(out)
}

##############
### script ###
##############
#load sample attribute 
at = read.table("Os2015/Os2015_input/SampleAttribute_2015_Takatsuki.txt", header = T, stringsAsFactors = F, fill = T )
CSSLs = unique(at$LineName)[grep("SL", unique(at$LineName))]
at=at[is.element(at$LineName, c("Koshihikari","Takanari",CSSLs)),]
rownames(at) = at[,"sampleID"]

# Substruct sample with reliable LineName prediction
load("Os2015/rawcnt-filtered_Osa_2015")
log2rpm = log2(t(t(rawcnt)/colSums(rawcnt)*10^6+0.1))
log2rpm = log2rpm[,is.element(colnames(log2rpm), rownames(at))]

# sample correlation summay -------------------------------------------------
ng.PlotCor <- function(rpm, rct, dr=drr, sample.set.use="all", 
                       use.color=ng.po.colors(64), llrange=(-2):3){
  for(i in llrange){
    lowerlimit <- i
    tmp <- log2(rpm[dr&(rowSums(rct)/ncol(rct)>10^lowerlimit),]+1)
    x <- cor(tmp)
    
    tmp.m <- sprintf("%s sample correlaton (%s genes with > 10^%s reads)", 
                     sample.set.use, nrow(tmp), i)
    image.plot(1:nrow(x), 1:ncol(x), x, main=tmp.m, col=use.color)
    
    x[x<0.6] <- 0.6
    image.plot(1:nrow(x), 1:ncol(x), x, main="sample correlaton (< 0.6 replaced to 0.6)", col=use.color)
    
    cat(dim(tmp))
    cat("\n")
  }    
}

#Define month borders
sample.at = at[colnames(log2rpm),]
month = table(sample.at$month)
day = table(sprintf("%s-%s",sample.at$month,sample.at$day))
border = c(0,(cumsum(month)+0.5)[1:4],869)
border.day = c(0,(cumsum(day)+0.5)[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30)],869)


#Prep. Fig for sample correlation
x = cor(log2rpm)
pdf("Figures/fig1d.pdf")
image.plot(1:nrow(x), 1:ncol(x), x, main="All sample correlation", col=ng.po.colors(64),xaxt="n", yaxt="n", xlab = "", ylab = "")
text(0,869,".")  # This is dummy, but required for drawing axis
axis(side = 1, at = border, labels = c("","","","","",""))
axis(side = 2, at = border, labels = c("","","","","",""))
abline(v = border.day, col = "white")
abline(h = border.day, col = "white")
dev.off()

#####################################Sup Fig2.b
# Caluculate flowring age
at = read.table("Os2015/Os2015_input/SampleAttribute_2015_Takatsuki.txt", header = T, stringsAsFactors = F, fill = T )
rownames(at) = at[,1]
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

# Genotype label
sample.kos = rownames(flowringAge.ave)
names(sample.kos) = rownames(flowringAge.ave)
sample.tak = sample.kos
lineIDkos <- unique(c("Koshihikari","Koshihikari_2",at$LineName[grep("SL12",at$LineName)]))
lineIDtak <- unique(c("Takanari","Takanari_2",at$LineName[grep("SL13",at$LineName)]))
sample.pkos <- is.element(sample.kos, c("Koshihikari","Koshihikari_2"))
sample.ptak <- is.element(sample.tak, c("Takanari","Takanari_2"))
sample.kos <- is.element(sample.kos, lineIDkos)
sample.tak <- is.element(sample.tak, lineIDtak)

pdf("Figures/supfig2b.pdf")
for(i in 1:4){
  layout(matrix(c(1,2), ncol = 1))
  hist(flowringAge.ave[sample.kos,i], breaks = seq(65,120,1), col ="blue",yaxt = "n",main = sprintf("SeedSowingDate : %s",SeedSowingDateList[i]), xlab = "Ave. flowringAge")
  hist(flowringAge.ave[sample.tak,i], breaks = seq(65,120,1), col ="red",yaxt = "n", main = NULL, xlab = NULL)
}
dev.off()

#####################################Sup Fig2.c
# Caluculate flowring age
at = read.table("Os2016/Os2016_input/SampleAttribute_2016_Kizu.txt", header = T, stringsAsFactors = F, fill = T )
rownames(at) = at[,1]
flowringDateList = read.table("Os2016/Os2016_input/2016CSSLs_headingDate.txt", header = T, stringsAsFactors = F)
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


# Genotype label
sample.kos = rownames(flowringAge.ave)
names(sample.kos) = rownames(flowringAge.ave)
sample.tak = sample.kos
lineIDkos <- unique(c("Koshihikari","Koshihikari_2",at$LineName[grep("SL12",at$LineName)]))
lineIDtak <- unique(c("Takanari","Takanari_2",at$LineName[grep("SL13",at$LineName)]))
sample.pkos <- is.element(sample.kos, c("Koshihikari","Koshihikari_2"))
sample.ptak <- is.element(sample.tak, c("Takanari","Takanari_2"))
sample.kos <- is.element(sample.kos, lineIDkos)
sample.tak <- is.element(sample.tak, lineIDtak)

pdf("Figures/supfig2c.pdf")
for(i in 1:2){
  layout(matrix(c(1,2), ncol = 1))
  hist(flowringAge.ave[sample.kos,i], breaks = seq(65,120,1), col ="blue",yaxt = "n",main = sprintf("SeedSowingDate : %s",SeedSowingDateList[i]), xlab = "Ave. flowringAge")
  hist(flowringAge.ave[sample.tak,i], breaks = seq(65,120,1), col ="red",yaxt = "n", main = NULL, xlab = NULL)
}
dev.off()

########################Prepare FIT input files
#load transcript description --------------------
load("General_input/141103-1_GeneDescription_Osa")

#load sample attribute 
at = read.table("Os2015/Os2015_input/SampleAttribute_2015_Takatsuki.txt", header = T, stringsAsFactors = F)
rownames(at) = at$sampleID
at.all = at


# load expression data table 
load("Os2015/rawcnt-filtered_Osa_2015")
log2rpm = log2(t(t(rawcnt)/colSums(rawcnt))*10^6+0.1)
log2rpm = log2rpm[,is.element(colnames(log2rpm),rownames(at))]
at = at.all[colnames(log2rpm),]

#Lable samples based on prediction from SNPs
lineIDkos <- unique(c("Koshihikari",at$LineName[grep("SL12",at$LineName)]))
lineIDtak <- unique(c("Takanari",at$LineName[grep("SL13",at$LineName)]))
sample.kos <- is.element(at$LineName, lineIDkos)
sample.tak <- is.element(at$LineName, lineIDtak)
sample.all <- is.element(at$LineName, c(lineIDtak, lineIDkos, "HP-a", "HP-b"))

# prepare precision weight matrix like as voom
tt <- with(at, sprintf("%s-%02d-%02d %02d:00:00", year, month, day, hour))
st <- strptime(tt, "%Y-%m-%d %H:%M:%S")
time.course = order(st)

residual.sd <- rep(0, nrow(log2rpm))
sp.predict <- matrix(0, nrow(log2rpm), ncol(log2rpm))
mean.log.count <- rep(0, nrow(log2rpm))
mean.lib.size <- mean(log2(colSums(rawcnt)))+ 1
for(i in 1:nrow(log2rpm)){
  gene.rpm = log2rpm[i,]
  gene.rpm.kos = gene.rpm[sample.kos]
  time.course.kos = time.course[sample.kos]
  time.course.kos = time.course.kos[gene.rpm.kos>min(log2rpm)] #Remove rpm = 0 for interpolation
  gene.rpm.kos = gene.rpm.kos[gene.rpm.kos>min(log2rpm)] #Remove rpm = 0 for interpolation
  
  gene.rpm.tak = gene.rpm[sample.tak]
  time.course.tak = time.course[sample.tak]
  time.course.tak = time.course.tak[gene.rpm.tak>min(log2rpm)] #Remove rpm = 0 for interpolation
  gene.rpm.tak = gene.rpm.tak[gene.rpm.tak>min(log2rpm)] #Remove rpm = 0 for interpolation
  
  sp.kos <- smooth.spline(time.course.kos,gene.rpm.kos) #Interpolation
  sp.tak <- smooth.spline(time.course.tak,gene.rpm.tak) #Interpolation

    #Caluculate SD of resudual error for each gene
  sp.predict.kos = predict(sp.kos, time.course)$y
  sp.predict.tak = predict(sp.tak, time.course)$y
  
  sp.predict[i,sample.kos] = sp.predict.kos[sample.kos]
  sp.predict[i,sample.tak] = sp.predict.tak[sample.tak]
  residual.sd[i] = sqrt(sd(sp.predict[i,] -gene.rpm))
  
  #Caluculate the average log-count for each gene
  mean.log.count[i] = mean(gene.rpm) + mean.lib.size - 6*log2(10)
  cat(i)
  cat("\n")
}

#Predict a trend line for residual sd by LOWESS
lo = lowess(mean.log.count, residual.sd)

#sp.predict(rpm) to sp.predict(count)
rawcnt = rawcnt[,rownames(at)]
log.library.size = matrix(log2(colSums(rawcnt)+1),byrow = T, ncol = ncol(sp.predict), nrow = nrow(sp.predict))
sp.predict.count = sp.predict + log.library.size -6*log2(10)

#Complement values between points and out of range (Inf) predicted by LOWESS 
lo.f<-approxfun(c(min(sp.predict.count), lo$x[lo$y>=min(residual.sd)], max(sp.predict.count)), c(max(residual.sd), lo$y[lo$y>=min(residual.sd)], min(residual.sd)))

plot(mean.log.count, residual.sd, xlim=c(min(mean.log.count),max(mean.log.count)), ylim = c(0,5))
lines(lo$x,lo$y, col=2)

#calculate sd of counts of each gene at each timepoint and convert to weights.
sp.predict.sd = matrix(lo.f(sp.predict.count),nrow = nrow(sp.predict.count),ncol = ncol(sp.predict.count))
voom.weigths = sp.predict.sd^(-4)
colnames(voom.weigths) = colnames(log2rpm)
rownames(voom.weigths) = rownames(log2rpm)
save(voom.weigths, file="Os2015/2015_weight_mat")


# make attribute matrix -----------------
source("scripts/scaled_age_2015.R")
SeedSowingDateList <- c("2015-04-16", "2015-04-30", "2015-05-14", "2015-05-28")
TransPlantDateList <- c("2015-05-14", "2015-05-21", "2015-06-04", "2015-06-18")
SeedSowingDate <- SeedSowingDateList[at$TransPlantSet]
TransPlantDate <- TransPlantDateList[at$TransPlantSet]
SamplingDate <- sprintf("%s-%02d-%02d", at[,"year"], at[,"month"], at[,"day"])
DayAfterSowing <- as.numeric(as.Date(SamplingDate) - as.Date(SeedSowingDate))
DayAfterTransplant <- as.numeric(as.Date(SamplingDate) - as.Date(TransPlantDate))
age = at$TransPlantSet
for(i in 1:nrow(at)){
  age[i] <- pesudoAge(c(SamplingDate[i],at$LineName[i],at$TransPlantSet[i]))
}

tt <- with(at, sprintf("%s-%02d-%02d %02d:00:00", year, month, day, hour))
st <- strptime(tt, "%Y-%m-%d %H:%M:%S")
st.ori <- strptime("2015-01-01 00:00:00", "%Y-%m-%d %H:%M:%S")
time <- as.numeric(st)/60 - as.numeric(st.ori)/60 +1

type <- rep(0, nrow(at))
min = rep(0, nrow(at))
at <- cbind(time, at, age, DayAfterSowing, DayAfterTransplant, SeedSowingDate, TransPlantDate, type, min)
at.all = at

log2rpm.all = t(log2rpm)
log2rpm.all = log2rpm.all[rownames(at.all),]
save(log2rpm.all, file = "Os2015/Os2015_FIT_input/log2rpm_all")
save(at.all, file = "Os2015/at_all")

at <- at.all[sample.all,]
save(at, file = "Os2015/Os2015_FIT_input/SampleAttribute_all")
pdf("Figures/supfig1d.pdf")
barplot(table(at$LineName), col = "gray")
dev.off()

at <- at.all[sample.kos, ]
save(at,  file = "Os2015/Os2015_FIT_input/SampleAttribute_kos")

at <- at.all[sample.tak, ]
save(at, file =  "Os2015/Os2015_FIT_input/SampleAttribute_tak")


log2rpm <- log2rpm.all[sample.all,]
save(log2rpm, file="Os2015/Os2015_FIT_input/rpm_all")
weights = t(voom.weigths)[sample.all,]
save(weights, file = "Os2015/Os2015_FIT_input/wm_all")

log2rpm <- log2rpm.all[sample.kos,]
save(log2rpm, file="Os2015/Os2015_FIT_input/rpm_kos")
weights = t(voom.weigths)[sample.kos,]
save(weights, file = "Os2015/Os2015_FIT_input/wm_kos")

log2rpm <- log2rpm.all[sample.tak,]
save(log2rpm, file="Os2015/Os2015_FIT_input/rpm_tak")
weights = t(voom.weigths)[sample.tak,]
save(weights, file = "Os2015/Os2015_FIT_input/wm_tak")

write.table(colnames(log2rpm.all), file = "Os2015/Os2015_FIT_input/gene_list", row.names = FALSE, col.names = FALSE, quote = FALSE)

##############################################Run FIT to contruct BG-based model
genes = colnames(log2rpm.all)
library("FIT")
#kos-BG model
#Load attribute information
training.attribute  <- FIT::load.attribute("Os2015/Os2015_FIT_input/SampleAttribute_kos", 'at')

#Load wether information
training.weather    <- FIT::load.weather("Os2015/Os2015_FIT_input/wtdata_2015_Takatsuki", 'wt', c('temperature', 'radiation'))

#Load expression data
training.expression <- FIT::load.expression("Os2015/Os2015_FIT_input/rpm_kos", 'log2rpm',genes)

#Load weight data
training.weight <- FIT::load.weight("Os2015/Os2015_FIT_input/wm_kos", 'weights',genes)

#Specify grid
grid.coords <- list(
  env.temperature.threshold = c(10, 15, 20, 25, 30),
  env.temperature.amplitude = c(-100/30, -1/30, 1/30, 100/30),
  env.radiation.threshold = c(1, 10, 20, 30, 40),
  env.radiation.amplitude = c(-100/80, -1/80, 1/80, 100/80),  
  env.temperature.period = c(10, 30, 90, 270, 720, 1440, 1440*3),
  env.radiation.period = c(10, 30, 90, 270, 720, 1440, 1440*3),
  gate.temperature.phase = seq(0, 23*60, 1*60),
  gate.radiation.phase = seq(0, 23*60, 1*60),
  gate.temperature.threshold = cos(pi*seq(4,24,4)/24),
  gate.radiation.threshold = cos(pi*seq(4,24,4)/24),
  gate.temperature.amplitude = c(-5, 5),
  gate.radiation.amplitude = c(-5, 5)
)

#Make the recipe
recipe.rnaseq <- FIT::make.recipe(c('temperature', 'radiation'), 
                                  init = 'gridsearch',
                                  optim = c('lm'),
                                  fit = 'fit.lasso',
                                  init.data = grid.coords,
                                  time.step = 10,
                                  gate.open.min = 480)

#Construct FIT modele
models.rnaseq <- unlist(FIT::train(training.expression,
                                   training.attribute,
                                   training.weather,
                                   recipe.rnaseq,
                                   training.weight))

#Save the model
save(models.rnaseq, file = "Os2015/Os2015_FIT_output/kos_BG_model")

#tak-BG model
#Load attribute information
fn = sprintf("%s",attribute)
training.attribute  <- FIT::load.attribute("Os2015/Os2015_FIT_input/SampleAttribute_tak", 'at')

#Load wether information
training.weather    <- FIT::load.weather("Os2015/Os2015_FIT_input/wtdata_2015_Takatsuki", 'wt', c('temperature', 'radiation'))

#Load expression data
training.expression <- FIT::load.expression("Os2015/Os2015_FIT_input/rpm_tak", 'log2rpm',genes)

#Load weight data
training.weight <- FIT::load.weight("Os2015/Os2015_FIT_input/wm_tak", 'weights',genes)

#Specify grid
grid.coords <- list(
  env.temperature.threshold = c(10, 15, 20, 25, 30),
  env.temperature.amplitude = c(-100/30, -1/30, 1/30, 100/30),
  env.radiation.threshold = c(1, 10, 20, 30, 40),
  env.radiation.amplitude = c(-100/80, -1/80, 1/80, 100/80),  
  env.temperature.period = c(10, 30, 90, 270, 720, 1440, 1440*3),
  env.radiation.period = c(10, 30, 90, 270, 720, 1440, 1440*3),
  gate.temperature.phase = seq(0, 23*60, 1*60),
  gate.radiation.phase = seq(0, 23*60, 1*60),
  gate.temperature.threshold = cos(pi*seq(4,24,4)/24),
  gate.radiation.threshold = cos(pi*seq(4,24,4)/24),
  gate.temperature.amplitude = c(-5, 5),
  gate.radiation.amplitude = c(-5, 5)
)

#Make the recipe
recipe.rnaseq <- FIT::make.recipe(c('temperature', 'radiation'), 
                                  init = 'gridsearch',
                                  optim = c('lm'),
                                  fit = 'fit.lasso',
                                  init.data = grid.coords,
                                  time.step = 10,
                                  gate.open.min = 480)


#Construct FIT modele
models.rnaseq <- unlist(FIT::train(training.expression,
                                   training.attribute,
                                   training.weather,
                                   recipe.rnaseq,
                                   training.weight))

#Save the model
save(models.rnaseq, file = "Os2015/Os2015_FIT_output/tak_BG_model")

################################# Prediction of expression at observed timepoints and genotype in 2015
#Load attribute information
prediction.attribute  <- FIT::load.attribute("Os2015/Os2015_FIT_input/SampleAttribute_all", 'at')

#Load wether information
prediction.weather    <- FIT::load.weather("Os2015/Os2015_FIT_input/wtdata_2015_Takatsuki", 'wt', c('temperature', 'radiation'))

#Load used.gene and attribute
load("Os2015/Os2015_FIT_input/rpm_all")
load("Os2015/Os2015_FIT_input/SampleAttribute_all")

#Predict with kos-BG model
load("Os2015/Os2015_FIT_output/kos_BG_model")
prediction <- FIT::predict(models.rnaseq, prediction.attribute, prediction.weather)
log2rpm.prediction.kos = t(matrix(unlist(prediction),byrow = T, nrow = length(prediction)))
colnames(log2rpm.prediction.kos) = colnames(log2rpm)
rownames(log2rpm.prediction.kos) = rownames(log2rpm)
save(log2rpm.prediction.kos, file = "Os2015/Os2015_FIT_output/kos_BG_model_prediction")

#Predict with tak-BG model
load("Os2015/Os2015_FIT_output/tak_BG_model")
prediction <- FIT::predict(models.rnaseq, prediction.attribute, prediction.weather)
log2rpm.prediction.tak = t(matrix(unlist(prediction),byrow = T, nrow = length(prediction)))
colnames(log2rpm.prediction.tak) = colnames(log2rpm)
rownames(log2rpm.prediction.tak) = rownames(log2rpm)
save(log2rpm.prediction.tak, file = "Os2015/Os2015_FIT_output/tak_BG_model_prediction")

################################# Prediction of expression at observed timepoints in Koshihikari and Takanari
source("scripts/scaled_age_2015.R")
##############Prepare attribute for prediction of Koshihikari and Takanari expression dinamics at all timepoints.
# Caluculate age for all timepoints
load("Os2015/Os2015_FIT_input/rpm_all")
load("Os2015/Os2015_FIT_input/SampleAttribute_all")

# Prep. timepoints for caluculating pseudoAge
load("Os2015/Sampling_timepoints_2015")
remove_hour = function(x){
  return(sprintf("%s-%s-%s",x[1],x[2],x[3]))
}
tmp = unlist(lapply(strsplit(names(timepoints),"-"), FUN = remove_hour))
names(tmp) = names(timepoints)
timepoints= tmp

# Prep. Date for caluculating pseudoAge
SeedSowingDateList <- c("2015-04-16", "2015-04-30", "2015-05-14", "2015-05-28")
TransPlantDateList <- c("2015-05-14", "2015-05-21", "2015-06-04", "2015-06-18")
SamplingDate <- timepoints

# Summarize observed points
age = as.numeric(SamplingDate)
names(age) = names(SamplingDate)
SamplingPoints = cbind(age,age,age,age)
colnames(SamplingPoints) = 1:4
for(i in 1:4){
  tmp = at[at$TransPlantSet==i,]
  tmp = unique(sprintf("%04d-%02d-%02d-%02d", tmp$year, tmp$month, tmp$day, tmp$hour))
  SamplingPoints[tmp,i] = 1
  range = which(!is.na(SamplingPoints[,i]))
  SamplingPoints[min(range):max(range),i] = 1
}

# Predict expression dynamics of each genotype
# Koshihikari prediction
lineName = "Koshihikari"
age = as.numeric(SamplingDate)
names(age) = names(SamplingDate)
age = cbind(age,age,age,age)
colnames(age) = 1:4
for(i in 1:length(SamplingDate)){
  for(TS in 1:4){
    age[i,TS] <- pesudoAge(c(SamplingDate[i],lineName,TS))
  }
}
age[is.na(SamplingPoints)] = NA

# Prepare attribute with all possible ages 
at.prediction = list()
for(n in 1 : nrow(at)){
  tmp = as.matrix(at[n,])
  # rownames(tmp) = NULL
  # tmp = as.vector(tmp)
  for(i in 1:4){
    # 8 #year
    # 9 #month
    # 10 #day
    # 11 #hour
    # 23 #age
    tmp[7] = i
    tmp[21] = age[sprintf("%04d-%02d-%02d-%02d",as.integer(tmp[8]), as.integer(tmp[9]), as.integer(tmp[10]), as.integer(tmp[11])),i]
    if(!is.na(tmp[21])){
      at.prediction = c(at.prediction,list(as.numeric(tmp[c(1,7:11,21,26:27)])))
    }
  }
}
at.prediction = unique(at.prediction)
tmp = NULL
for(i in 1: length(at.prediction)){
  tmp= rbind(tmp,at.prediction[[i]])
}
colnames(tmp) = colnames(at)[c(1,7:11,21,26:27)]
at.FIT = data.frame(tmp, stringsAsFactors = F)
at.FIT = at.FIT[order(at.FIT$time),]
at.FIT.kos = at.FIT
save(at.FIT, file = "Os2015/Os2015_FIT_input/attribute_for_prediction")

#Load attribute information
prediction.attribute  <- FIT::load.attribute("Os2015/Os2015_FIT_input/attribute_for_prediction", 'at.FIT')
#Load wether information
prediction.weather    <- FIT::load.weather("Os2015/Os2015_FIT_input/wtdata_2015_Takatsuki", 'wt', c('temperature', 'radiation'))
###Load data, merge and save
load("Os2015/Os2015_FIT_output/kos_BG_model")
prediction <- FIT::predict(models.rnaseq, prediction.attribute, prediction.weather)
log2rpm.prediction.kos = t(matrix(unlist(prediction),byrow = T, nrow = length(prediction)))
colnames(log2rpm.prediction.kos) = colnames(log2rpm)

# Takanari prediction
lineName = "Takanari"
age = as.numeric(SamplingDate)
names(age) = names(SamplingDate)
age = cbind(age,age,age,age)
colnames(age) = 1:4
for(i in 1:length(SamplingDate)){
  for(TS in 1:4){
    age[i,TS] <- pesudoAge(c(SamplingDate[i],lineName,TS))
  }
}
age[is.na(SamplingPoints)] = NA

# Prepare attribute with all possible ages 
at.prediction = list()
for(n in 1 : nrow(at)){
  tmp = as.matrix(at[n,])
  # rownames(tmp) = NULL
  # tmp = as.vector(tmp)
  for(i in 1:4){
    # 8 #year
    # 9 #month
    # 10 #day
    # 11 #hour
    # 23 #age
    tmp[7] = i
    tmp[21] = age[sprintf("%04d-%02d-%02d-%02d",as.integer(tmp[8]), as.integer(tmp[9]), as.integer(tmp[10]), as.integer(tmp[11])),i]
    if(!is.na(tmp[21])){
      at.prediction = c(at.prediction,list(as.numeric(tmp[c(1,7:11,21,26:27)])))
    }
  }
}
at.prediction = unique(at.prediction)
tmp = NULL
for(i in 1: length(at.prediction)){
  tmp= rbind(tmp,at.prediction[[i]])
}
colnames(tmp) = colnames(at)[c(1,7:11,21,26:27)]
at.FIT = data.frame(tmp, stringsAsFactors = F)
at.FIT = at.FIT[order(at.FIT$time),]
at.FIT.tak = at.FIT
save(at.FIT, file = "Os2015/Os2015_FIT_input/attribute_for_prediction")

#Load attribute information
prediction.attribute  <- FIT::load.attribute("Os2015/Os2015_FIT_input/attribute_for_prediction", 'at.FIT')
###Load data, merge and save
load("Os2015/Os2015_FIT_output/tak_BG_model")
prediction <- FIT::predict(models.rnaseq, prediction.attribute, prediction.weather)
log2rpm.prediction.tak = t(matrix(unlist(prediction),byrow = T, nrow = length(prediction)))
colnames(log2rpm.prediction.tak) = colnames(log2rpm)
save(log2rpm.prediction.kos, log2rpm.prediction.tak, at.FIT.kos, at.FIT.tak, file = sprintf("Os2015/Os2015_FIT_output/kostak_prediction",lineName))

###################################### edQTL detection
library("fitdistrplus")

# load attribute data --------------
load("Os2015/Os2015_FIT_input/attribute_for_prediction")
#load transcript description 
load("General_input/141103-1_GeneDescription_Osa")
# load genotype matrix
gtmat <- read.delim("General_input/161117_Genotype_KosTakCSSL_HPa12.txt", header=T, as.is=T)
gt.at <- gtmat[,1:4]
gt.mat <- gtmat[,5:ncol(gtmat)] 

#Load FIT prediction result
# calc distance -----------------
load("Os2015/Os2015_FIT_output/kostak_prediction")

######### Caluculate EDP
dist <- sqrt(colSums((log2rpm.prediction.kos - log2rpm.prediction.tak)^2)) # euclidean distance of each gene
names(dist) = colnames(log2rpm.prediction.kos)

pdf("Figures/supfig1e.pdf")
hist(dist,col="gray", breaks = "Scott")
abline(v=40, col = "red")
dev.off()

crit <- dist>40
gnl_high = names(sort(dist[crit], decreasing = T))
gnl_high <- gnl_high[grep("Os[0-9][0-9]g",gnl_high)]
save(gnl_high, file = "Os2015/gnl_high")

# Load attribute of observed sample
load("Os2015/Os2015_FIT_input/SampleAttribute_all")
# Load FIT prediction result in observed sample
load("Os2015/Os2015_FIT_output/kos_BG_model_prediction")
load("Os2015/Os2015_FIT_output/tak_BG_model_prediction")

# genotype --------------------
at = at[grep("HP",at$LineName, invert = T),]
lineIDkos <- unique(c("Koshihikari",at$LineName[grep("SL12",at$LineName)]))
lineIDtak <- unique(c("Takanari",at$LineName[grep("SL13",at$LineName)]))
sample.kos <- is.element(at$LineName, lineIDkos)
sample.tak <- is.element(at$LineName, lineIDtak)
sample.all <- is.element(at$LineName, c(lineIDtak, lineIDkos))


# load expression data table
load("Os2015/Os2015_FIT_input/log2rpm_all")
log2rpm = log2rpm.all[rownames(at),]
log2rpm.all = log2rpm

#Kos obeserved exp - Kos or Tak predicted exp
rsd.kos <- abs(log2rpm.prediction.kos[rownames(at),] -log2rpm.all)
#Tak obeserved exp - Kos or Tak predicted exp
rsd.tak <- abs(log2rpm.prediction.tak[rownames(at),] - log2rpm.all)

#residual based on backgrond genotype
#kos
kos = apply(rsd.kos[sample.kos,],2,mean)
#tak
tak = apply(rsd.tak[sample.tak,],2,mean)
residual.current = list(kos = kos, tak = tak)

linename <- colnames(gt.mat)
gt <- at[,"LineName"]
koskos <- matrix(NA, nrow=ncol(rsd.kos), ncol=nrow(gt.mat)) #genes x makers(A:Kos, B:Tak)
rownames(koskos) <-colnames(rsd.kos)
taktak <- koskos
for(i in 1:nrow(gt.mat)){ # i indicates Line
  marker <- gt.mat[i,]
  #Kos obeserved exp - Kos predicted exp
  koskos[,i] <- apply(rsd.kos[is.element(gt, linename[marker=="A"]),], 2, mean)
  #Tak obeserved exp - Tak predicted exp
  taktak[,i] <- apply(rsd.tak[is.element(gt, linename[marker=="B"]),], 2, mean)
  cat(sprintf("%s : %s / %s\n", Sys.time(), i, nrow(gt.mat)))  
}
dif.gt <- list(koskos=koskos,taktak=taktak) #residual error between observation and prediction in all combination)
save(dif.gt, file="Os2015/dif_all.gt")

# FiT-QTL detection ---------------------------------------
#######Examine whether Markers significantly effect gene expression
# Calculate residual errors with 1000 times permutation test against sample IDs
niter <- 1000
dif.sim <- NULL
rsd.kos = rsd.kos[,gnl_high]
rsd.tak = rsd.tak[,gnl_high]
seed = 1234
set.seed(seed)
for(i in 1:niter){
  gt <- at[,"LineName"]
  #Changing order of samples of varied line among samples with same background
  gt[sample.kos] <- sample(gt[sample.kos])
  gt[sample.tak] <- sample(gt[sample.tak])
  
  random_kos <- matrix(NA, nrow=ncol(rsd.kos), ncol=nrow(gt.mat))
  rownames(random_kos) <- colnames(rsd.kos)
  random_tak <- random_kos
  
  for(j in 1:nrow(gt.mat)){
    marker <- gt.mat[j,] # Check the genotype (kos/tak) of each marker
    # obeserved exp - Kos predicted exp
    random_kos[,j] <- apply(rsd.kos[is.element(gt, linename[marker=="A"]),], 2, mean)
    # obeserved exp - Tak predicted exp
    random_tak[,j] <- apply(rsd.tak[is.element(gt, linename[marker=="B"]),], 2, mean)
    
  }
  dif.sim <- c(dif.sim, list(random_kos+random_tak))
  cat(sprintf("%s : %s / %s\n", Sys.time(), i, niter))
}
#Convert the list of test(samle) to each gene
dif.sim2 <- NULL
for(i in 1:length(gnl_high)){
  tmp <- lapply(dif.sim, FUN = function(x){x[i,]})
  dif.sim2 <- c(dif.sim2, list(tmp))
}
names(dif.sim2) <- gnl_high
dif.sim <- dif.sim2
save(dif.sim, file=sprintf("Os2015/dif_all_%s_%s.sim", niter, seed))

pdf("Figures/supfig3bc.pdf", width = 14)
gn = "Os09g0343200"
layout(matrix(c(1,2), ncol = 2))
sum.kostak = dif.gt$koskos[gn,]+dif.gt$taktak[gn,]
improvement = mean(rsd.kos[sample.kos,gn])+mean(rsd.tak[sample.tak,gn])- sum.kostak
range = sort(c(0.05,improvement))
range = c(range[1],range[length(range)])
col = rep("black", length(improvement))
col[improvement==max(improvement)] = "orange"
cex = rep(1,length(col))
cex[improvement==max(improvement)] = 1.5
plot(improvement, ylab = "Improvement of R. E.", main=sprintf("  %s : %s : %s", gn, des[gn,3], des[gn,4]), pch = 16, sub = des[gn,2], xlab = "Markers", ylim = range, col = col, cex = cex)
abline(h = 0, lty = "dashed")
abline(v=c(0,cumsum(table(gt.at$chr.)))+0.5, col="#666666", lty="dotted")
border = mean(rsd.kos[sample.kos,gn])+mean(rsd.tak[sample.tak,gn])
res = unlist(dif.sim[[gn]])
fit = fitdist(res,"lnorm", "qme", probs = c(0.7, 0.01))
ml = fit$estimate[1]
sl = fit$estimate[2]
m = mean(res)
s = sd(res)
hist(res, xlim = rev(c(min(sum.kostak), m+4*s)), xaxt="n",yaxt="n", xlab = "Improvement of R. E. (Lines indicate the top 1% borders)", main=sprintf("  %s : %s : %s", gn, des[gn,3], des[gn,4]), sub = des[gn,2])
par(new=T)
curve(dlnorm(x, meanlog = ml, sdlog = sl), m+4*s,min(sum.kostak),xlim = rev(c(min(sum.kostak), m+4*s)),col="magenta", xlab="", ylab="",xaxt="n")
axis(side = 1, at = round(seq(min(sum.kostak), m+4*s,length.out = 12),1), labels = round(seq(-min(sum.kostak)+border,-(m+4*s)+ border, length.out = 12),3))
abline(h = 0)
abline(v = quantile(res, 0.001))
abline(v = qlnorm(0.001, meanlog  = ml, sdlog  = sl), col = "magenta")
abline(v = min(sum.kostak), col = "orange", lwd = 3)
dev.off()


#Caluculate p-value and q-value
p.value = matrix(1, ncol = ncol(log2rpm), nrow = nrow(gt.mat))
colnames(p.value) = colnames(log2rpm)
q.value = p.value
quantile.VS.lnorm = rep(0,length(gnl_high))

for(i in 1:length(gnl_high)){
  gn <- gnl_high[i]
  kos <- dif.gt$koskos[gn,]
  tak <- dif.gt$taktak[gn,]
  sum.kostak <- kos+tak
  #caluculate mean and var
  res = unlist(dif.sim[[gn]])
  fit = fitdist(res,"lnorm", "qme", probs = c(0.7, 0.01))
  ml = fit$estimate[1]
  sl = fit$estimate[2]
  quantile.VS.lnorm[i] = qlnorm(0.001, meanlog = ml, sdlog = sl)-quantile(res, 0.001) # <0 indicates consavative
  p.value[,gn] = plnorm(sum.kostak, meanlog = ml, sdlog = sl)
  cat(sprintf("%s : %s / %s, qlnorm %s, lnorm %s, diff %s \n", Sys.time(), i, length(gnl_high), qlnorm(0.001, meanlog = ml, sdlog = sl),quantile(res, 0.001),quantile.VS.lnorm[i]))
}

q.value = p.adjust(p.value[,gnl_high],method = "BH")
q.value = matrix(q.value, nrow = nrow(gt.mat))
colnames(q.value) = gnl_high
save(p.value, q.value, file = "Os2015/pqvalue")

pdf("Figures/supfig3d.pdf")
plot(-quantile.VS.lnorm, ylab = "LogNorm(0.001) - Quantile(0.001)", xlab = "Genes", pch = 16, sub = sprintf(" Num. : %d", sum(quantile.VS.lnorm>0)))
dev.off()

#######Prep. edQTL-model prediction table
# load expression data table
load("Os2015/Os2015_FIT_input/log2rpm_all")

# load genotype matrix
fn.genotypemat <- "General_input/161117_Genotype_KosTakCSSL_HPa12.txt"
gtmat <- read.delim(fn.genotypemat, header=T, as.is=T)
gt.at <- gtmat[,1:4]
gt.mat <- gtmat[,5:ncol(gtmat)] 
gt.mat[gt.mat == "A/B"] == "B" #HP-a

# load sample attribute 
load("Os2015/Os2015_FIT_input/SampleAttribute_all")
at = at[grep("HP", at$LineName, invert = T),]
at = at[sprintf("%s",sort(at$sampleID)),]

# Load p-value and q-value
load("Os2015/pqvalue")

#Prep. edQTL list
FDR = 0.05
q.value.sig = q.value[,colSums(q.value<FDR)>0]
dim(q.value.sig)
genes_with_Markers = colnames(q.value.sig)

#Count genes regulated by edQTLs on the other chromosomes
gnl.trans.edQTL = NULL 
gnl.cis.trans.edQTL = NULL
gnl.multi.edQTL = NULL 
for(i in 1:ncol(q.value.sig)){
  gn = colnames(q.value.sig)[i]
  chr.gene = as.numeric(gsub("Os","", strsplit(gn, "g")[[1]][1]))
  markers = q.value.sig[,gn]
  chr.edQTL = unique(gtmat$chr.[markers<FDR])
  if(is.element(chr.gene, chr.edQTL)&length(chr.edQTL)>1){
    gnl.cis.trans.edQTL = c(gnl.cis.trans.edQTL, gn)
  } else if(!is.element(chr.gene, chr.edQTL)){
    gnl.trans.edQTL = c(gnl.trans.edQTL, gn)
  }
  if(length(chr.edQTL)>1){
    gnl.multi.edQTL = c(gnl.multi.edQTL,gn)
  }
}

length(gnl.trans.edQTL) #222
length(gnl.cis.trans.edQTL) #40
length(gnl.multi.edQTL) #43
save(gnl.trans.edQTL, gnl.cis.trans.edQTL, gnl.multi.edQTL, file = "Os2015/gnl-trans-edQTL")
significant.marker.list = list()
for(gn in genes_with_Markers){
  gn.Markers.q.vakue = q.value.sig[,gn]
  names(gn.Markers.q.vakue) = seq(1,length(gn.Markers.q.vakue))
  gn.Markers = gn.Markers.q.vakue<FDR
  # detect peaks 
  peak.pos = NULL
  before.negative = 1
  for(z in 1:length(gn.Markers)){
    if(gn.Markers[z]==TRUE && before.negative == 1){
      sp = z
      before.negative = 0
    } 
    if(gn.Markers[z]==FALSE && before.negative == 0){
      ep = z-1
      c(sp, ep)
      peak.pos = c(peak.pos, list(c(sp, ep)))
      before.negative = 1
    }
    if(z == length(gn.Markers) && before.negative == 0){
      ep = z
      peak.pos = c(peak.pos, list(c(sp, ep)))
    }
    
  }
  #detect most siginificant marker
  peak.pos.sig = NULL
  for(i in 1:length(peak.pos)){
    sp = peak.pos[[i]][1]
    ep = peak.pos[[i]][2]
    if(sp < ep){
      q.value.tmp = q.value[sp:ep,gn]
      q.value.min = min(q.value.tmp)
      peak.pos.sig = c(peak.pos.sig,(sp:ep)[q.value.tmp==q.value.min])   
    } else {
      peak.pos.sig = c(peak.pos.sig,sp)
    }
  }
  tmp = list(peak.pos.sig)
  names(tmp) = gn
  significant.marker.list = c(significant.marker.list,tmp)
}

#Prep. base.genetype list 
base.genotype = apply(gt.mat,2,FUN = table)
judge.base.genotype = function(x){
  return(names(x)[x==max(x)])
}
base.genotype = unlist(lapply(base.genotype, FUN = judge.base.genotype))
base.genotype.rev = base.genotype
for(i in 1:length(base.genotype.rev)){
  if(base.genotype.rev[i]=="A"){
    base.genotype.rev[i] = "B"
  } else {
    base.genotype.rev[i] = "A"
  }
}
prediction.model.mat = matrix(rep(base.genotype, ncol(log2rpm)),byrow = T, nrow = ncol(log2rpm), ncol = length(base.genotype))
rownames(prediction.model.mat) = colnames(log2rpm)
colnames(prediction.model.mat) = names(base.genotype)

for(i in 1:length(genes_with_Markers)){
  gn = genes_with_Markers[i]
  prediction.model.mat[gn,] = base.genotype
  flag = colSums(!gt.mat[significant.marker.list[[i]],] == matrix(rep(base.genotype, length(significant.marker.list[[i]])), byrow = T, nrow = length(significant.marker.list[[i]])))>0
  prediction.model.mat[gn,flag] = base.genotype.rev[flag]
}
colnames(prediction.model.mat)[83] = "HP-a"
colnames(prediction.model.mat)[84] = "HP-b"
save(FDR, prediction.model.mat, file = "Os2015/prediction_model_mat")

################
### function ###
################
library("fields")
requireNamespace("FIT")
library("plyr")

source("scripts/ng.Colors.R")
source("scripts/plot_time_course_DEG_dual.R")
source("scripts/colname2rgb.R")

blues = c("blue","Royalblue", "skyblue", "cyan") 
reds = c("red","red3","pink1","magenta")
blues = colnames2rgb(blues, 50)
reds = colnames2rgb(reds, 50)


ng.plot.ObsPred <- function(gn, ex, ks, np, des, col,col2,min,max, at, timepoints, at.prediction,DarkLight, day.border){
  at.time = timepoints[sprintf("%04d-%02d-%02d-%02d", at$year, at$month, at$day, at$hour)]
  plot(at.time,ex[,gn], ylim=c(min,max),
       m=sprintf("  %s : %s : %s", gn, des[gn,3], des[gn,4]),
       sub=des[gn,2], type ="n")
  for(i in 1:length(DarkLight)){
    polygon(DarkLight[[i]],c(max-1,max-1,max,max),col = "gray", border = NA)
  }  
  abline(v = day.border, lty = "dashed", col = "gray")
  points(at.time,ex[,gn], col=col, pch = 16)
  points(at.time,ex[,gn], col=col2, pch = 16)
  for(c in 4:1){
    lines(timepoints[sprintf("%04d-%02d-%02d-%02d", at.prediction$year, at.prediction$month, at.prediction$day, at.prediction$hour)[at.prediction$TransPlantSet==c]], np[at.prediction$TransPlantSet==c,gn], col=reds[c])
    lines(timepoints[sprintf("%04d-%02d-%02d-%02d", at.prediction$year, at.prediction$month, at.prediction$day, at.prediction$hour)[at.prediction$TransPlantSet==c]], ks[at.prediction$TransPlantSet==c,gn], col=blues[c])
  }
}

ng.plot.ObsPred.edQTL <- function(gn, ex, ks, np, des, col,col2,min,max, at, timepoints, at.prediction,DarkLight, day.border){
  at.time = timepoints[sprintf("%04d-%02d-%02d-%02d", at$year, at$month, at$day, at$hour)]
  plot(at.time,ex[,gn], ylim=c(min,max),
       m=sprintf("  %s : %s : %s", gn, des[gn,3], des[gn,4]),
       sub=des[gn,2], type ="n")
  for(i in 1:length(DarkLight)){
    polygon(DarkLight[[i]],c(max-1,max-1,max,max),col = "gray", border = NA)
  }  
  abline(v = day.border, lty = "dashed", col = "gray")
  points(at.time,ex[,gn], col=col, pch = 16)
  points(at.time,ex[,gn], col=col2, pch = 16)
  for(c in 4:1){
    lines(timepoints[sprintf("%04d-%02d-%02d-%02d", at.prediction$year, at.prediction$month, at.prediction$day, at.prediction$hour)[at.prediction$TransPlantSet==c]], np[at.prediction$TransPlantSet==c,gn], col=reds[c])
    lines(timepoints[sprintf("%04d-%02d-%02d-%02d", at.prediction$year, at.prediction$month, at.prediction$day, at.prediction$hour)[at.prediction$TransPlantSet==c]], ks[at.prediction$TransPlantSet==c,gn], col=blues[c])
  }
  pch = col2
  pch[col2=="#FFFFFF00"] = ""
  pch[col2!="#FFFFFF00"] = 25
  pch = as.numeric(pch)
  points(at.time,ex[,gn]+2,pch =pch, bg = "black", cex = 0.8)
  
}

############# Make Fig2
# load data --------------
# load sample attribute 
load("Os2015/Os2015_FIT_input/attribute_for_prediction")
at.prediction = at.FIT

# load sample attribute 
load("Os2015/Os2015_FIT_input/SampleAttribute_all")
at = at[grep("HP", at$LineName, invert = T),]

# load expression data table
load("Os2015/Os2015_FIT_input/log2rpm_all")
log2rpm = log2rpm.all[rownames(at),]
at = at[rownames(log2rpm),]

# genotype --------------------
lineIDkos <- unique(c("Koshihikari",at$LineName[grep("SL12",at$LineName)]))
lineIDtak <- unique(c("Takanari",at$LineName[grep("SL13",at$LineName)]))
sample.kos <- is.element(at$LineName, lineIDkos)
sample.tak <- is.element(at$LineName, lineIDtak)
sample.all <- is.element(at$LineName, c(lineIDtak, lineIDkos))
names(sample.all) = rownames(at)
names(sample.kos) = rownames(at)
names(sample.tak) = rownames(at)

#load transcript description 
load("General_input/141103-1_GeneDescription_Osa")

# Load timepoints 
load("Os2015/Sampling_timepoints_2015")

#Caluculate the border of days
remove_hour = function(x){
  return(sprintf("%s-%s-%s",x[1],x[2],x[3]))
}
day =unlist(lapply(strsplit(names(timepoints), "-"), FUN = remove_hour))
day.border = 0
pre = day[1]
first.day = TRUE
for(i in 1:length(day)){
  if(pre != day[i] && !first.day){
    day.border = c(day.border, i - 0.5)
    first.day = TRUE
    pre = day[i]
  } else if(pre != day[i]){
    first.day = FALSE
    pre = day[i]
    
  }
}
day.border = c(day.border,length(day))

# Load DarkLight
load("Os2015/DarkLight_Takatsuki_2015")

# Load FIT prediction matrix
load("Os2015/Os2015_FIT_output/kostak_prediction")

# Load genotype matrix
fn.genotypemat <- "General_input/161117_Genotype_KosTakCSSL_HPa12.txt"
gtmat <- read.delim(fn.genotypemat, header=T, as.is=T)
gt.at <- gtmat[,1:4]
gt.mat <- gtmat[,5:ncol(gtmat)] 
gt.mat[gt.mat == "A/B"] == "B" #HP-a

#####Load p-value and q-value
load("Os2015/pqvalue")

#Prep expression plot
gn = "Os09g0343200"
rpm.min = min(log2rpm)
rpm.max = max(log2rpm)

pch = sample.kos
pch[sample.kos==TRUE] = 19
pch[sample.tak==TRUE] = 19
pch[pch==FALSE] = 0

color.kos = sample.kos
color.kos[color.kos] = colnames2rgb("blue",100) 
color.kos[color.kos==FALSE] = colnames2rgb("white",0)

color.tak = sample.tak
color.tak[color.tak] = colnames2rgb("red",100) 
color.tak[color.tak==FALSE] = colnames2rgb("white",0)


color.bg = sample.kos
for(n in 1:4){
  color.bg[sample.kos+1==2&at$TransPlantSet==n] = colnames2rgb(blues[n],20)
}
for(n in 1:4){
  color.bg[sample.kos+1==1&at$TransPlantSet==n] = colnames2rgb(reds[n],20)
}

#Load predictied model matrix
load("Os2015/prediction_model_mat")
model.gn = prediction.model.mat[gn,]
color.edQTL = rep(colnames2rgb("white",0), length(sample.kos))
edQTL.model.gn = model.gn[at$LineName]
bg.model = rep("-", length(sample.kos))
bg.model[sample.kos] = "A"
bg.model[sample.tak] = "B"
for(n in 1:4){
  color.edQTL[(edQTL.model.gn!=bg.model)&(edQTL.model.gn=="A")&(at$TransPlantSet==n)] = colnames2rgb(blues[n],100)
}
for(n in 1:4){
  color.edQTL[(edQTL.model.gn!=bg.model)&(edQTL.model.gn=="B")&(at$TransPlantSet==n)] = colnames2rgb(reds[n],100)
}

color.kos = rep(colnames2rgb("white",0),length(sample.kos))
names(color.kos) = rownames(log2rpm)
color.tak = color.kos
color.kos[sample.kos] = blues[at[sample.kos,]$TransPlantSet]
color.tak[sample.tak] = reds[at[sample.tak,]$TransPlantSet]

# Make plot of Kos BG and Tak BG for Fig.2ab
pdf("Figures/fig2ac.pdf")
layout(matrix(c(1,2,3), byrow = T, ncol = 1))
ng.plot.ObsPred(gn, log2rpm, log2rpm.prediction.kos, log2rpm.prediction.tak, des, color.kos,color.kos,rpm.min,rpm.max,at, timepoints, at.prediction, DarkLight, day.border)  
ng.plot.ObsPred(gn, log2rpm, log2rpm.prediction.kos, log2rpm.prediction.tak, des, color.tak,color.tak,rpm.min,rpm.max,at, timepoints, at.prediction, DarkLight, day.border)  
ng.plot.ObsPred.edQTL(gn, log2rpm, log2rpm.prediction.kos, log2rpm.prediction.tak, des, color.bg,color.edQTL,rpm.min,rpm.max,at, timepoints, at.prediction, DarkLight, day.border)  
dev.off()

# Make edQTL detection
pdf("Figures/fig2b.pdf", height = 5)
col.q.value = rep("gray",nrow(gt.mat))
col.q.value[q.value[,gn]<FDR] = "red"
range = range(-log10(q.value[,gn]))
if(range[2]<6){
  range[2]=6
}
plot(-log10(q.value[,gn]),ylim = range, xlab = "marker", pch = 16)
lines(-log10(q.value[,gn]), ylim = range)
points(-log10(q.value[,gn]), col=col.q.value, ylim = range, pch =16)
abline(h = -log10(FDR))
abline(v=c(0,cumsum(table(gt.at$chr.)))+0.5, col="#666666", lty="dotted")
dev.off()

# Make edQTL map
load("Os2015/gnl-trans-edQTL")
pdf("Figures/fig2d.pdf")
gnl_high = colnames(q.value)[colSums(q.value<FDR)>0]
q.value.high = q.value[,sort(gnl_high)]
q.value.high[q.value.high<FDR] = 0
q.value.high[q.value.high>=FDR] = 1
q.value.high2 = as.numeric()
chr.gene = NULL
for(i in 1:ncol(q.value.high)){
  pos = as.integer(strsplit(sub("Os", "", colnames(q.value.high)[i]),"g")[[1]][1])
  chr.gene = c(chr.gene, pos)
  q.value.high2 = cbind(q.value.high2, q.value.high[,i])
  if(is.element(colnames(q.value.high)[i], c(gnl.cis.trans.edQTL, gnl.trans.edQTL))){
    for(n in 1:10){
      chr.gene = c(chr.gene, pos)
      q.value.high2 = cbind(q.value.high2, q.value.high[,i])
    }
  }
}
sep.chr <- (c(0,cumsum(table(gt.at$chr.)))) / nrow(gt.mat)
sep.gene <- (c(0,cumsum(table(chr.gene)))) / ncol(q.value.high2)
image.plot(q.value.high2, col=colorRampPalette(c("red",colnames2rgb("white",0)))(2),xlab="marker", ylab = "gene", lwd=10)
title(sub = "p>0.05 = 0.05, p<0.05/Num. of candidates=0.05/Num. of candidates")
abline(v=sep.chr, col="light gray")
abline(h=sep.gene, col="light gray")
dev.off()

# Make plot of Kos BG and Tak BG for Fig.2
gn = "Os01g0537250"
model.gn = prediction.model.mat[gn,]
color.edQTL = rep(colnames2rgb("white",0), length(sample.kos))
edQTL.model.gn = model.gn[at$LineName]
bg.model = rep("-", length(sample.kos))
bg.model[sample.kos] = "A"
bg.model[sample.tak] = "B"
for(n in 1:4){
  color.edQTL[(edQTL.model.gn!=bg.model)&(edQTL.model.gn=="A")&(at$TransPlantSet==n)] = colnames2rgb(blues[n],100)
}
for(n in 1:4){
  color.edQTL[(edQTL.model.gn!=bg.model)&(edQTL.model.gn=="B")&(at$TransPlantSet==n)] = colnames2rgb(reds[n],100)
}

color.kos = rep(colnames2rgb("white",0),length(sample.kos))
names(color.kos) = rownames(log2rpm)
color.tak = color.kos
color.kos[sample.kos] = blues[at[sample.kos,]$TransPlantSet]
color.tak[sample.tak] = reds[at[sample.tak,]$TransPlantSet]

pdf("Figures/fig2e.pdf")
layout(matrix(c(1,2,3), byrow = T, ncol = 1))
ng.plot.ObsPred(gn, log2rpm, log2rpm.prediction.kos, log2rpm.prediction.tak, des, color.kos,color.kos,rpm.min,rpm.max,at, timepoints, at.prediction, DarkLight, day.border)  
ng.plot.ObsPred(gn, log2rpm, log2rpm.prediction.kos, log2rpm.prediction.tak, des, color.tak,color.tak,rpm.min,rpm.max,at, timepoints, at.prediction, DarkLight, day.border)  
ng.plot.ObsPred.edQTL(gn, log2rpm, log2rpm.prediction.kos, log2rpm.prediction.tak, des, color.bg,color.edQTL,rpm.min,rpm.max,at, timepoints, at.prediction, DarkLight, day.border)  
dev.off()

# Make fig2f
pdf("Figures/fig2f.pdf", height = 5)
col.q.value = rep("gray",nrow(gt.mat))
col.q.value[q.value[,gn]<FDR] = "red"
range = range(-log10(q.value[,gn]))
if(range[2]<6){
  range[2]=6
}
plot(-log10(q.value[,gn]),ylim = range, xlab = "marker", pch = 16)
lines(-log10(q.value[,gn]), ylim = range)
points(-log10(q.value[,gn]), col=col.q.value, ylim = range, pch =16)
abline(h = -log10(FDR))
abline(v=c(0,cumsum(table(gt.at$chr.)))+0.5, col="#666666", lty="dotted")
dev.off()

#  Make plot of Kos BG and Tak BG for sup figure 9
gn = "Os10g0111700"
model.gn = prediction.model.mat[gn,]
color.edQTL = rep(colnames2rgb("white",0), length(sample.kos))
edQTL.model.gn = model.gn[at$LineName]
bg.model = rep("-", length(sample.kos))
bg.model[sample.kos] = "A"
bg.model[sample.tak] = "B"
for(n in 1:4){
  color.edQTL[(edQTL.model.gn!=bg.model)&(edQTL.model.gn=="A")&(at$TransPlantSet==n)] = colnames2rgb(blues[n],100)
}
for(n in 1:4){
  color.edQTL[(edQTL.model.gn!=bg.model)&(edQTL.model.gn=="B")&(at$TransPlantSet==n)] = colnames2rgb(reds[n],100)
}

color.kos = rep(colnames2rgb("white",0),length(sample.kos))
names(color.kos) = rownames(log2rpm)
color.tak = color.kos
color.kos[sample.kos] = blues[at[sample.kos,]$TransPlantSet]
color.tak[sample.tak] = reds[at[sample.tak,]$TransPlantSet]

# Make plot of Kos BG and Tak BG for sup fig9a
gn = "Os12g0406000"
model.gn = prediction.model.mat[gn,]
color.edQTL = rep(colnames2rgb("white",0), length(sample.kos))
edQTL.model.gn = model.gn[at$LineName]
bg.model = rep("-", length(sample.kos))
bg.model[sample.kos] = "A"
bg.model[sample.tak] = "B"
for(n in 1:4){
  color.edQTL[(edQTL.model.gn!=bg.model)&(edQTL.model.gn=="A")&(at$TransPlantSet==n)] = colnames2rgb(blues[n],100)
}
for(n in 1:4){
  color.edQTL[(edQTL.model.gn!=bg.model)&(edQTL.model.gn=="B")&(at$TransPlantSet==n)] = colnames2rgb(reds[n],100)
}

color.kos = rep(colnames2rgb("white",0),length(sample.kos))
names(color.kos) = rownames(log2rpm)
color.tak = color.kos
color.kos[sample.kos] = blues[at[sample.kos,]$TransPlantSet]
color.tak[sample.tak] = reds[at[sample.tak,]$TransPlantSet]

pdf("Figures/supfig9a.pdf")
layout(matrix(c(1,2,3), byrow = T, ncol = 1))
ng.plot.ObsPred(gn, log2rpm, log2rpm.prediction.kos, log2rpm.prediction.tak, des, color.kos,color.kos,rpm.min,rpm.max,at, timepoints, at.prediction, DarkLight, day.border)  
ng.plot.ObsPred(gn, log2rpm, log2rpm.prediction.kos, log2rpm.prediction.tak, des, color.tak,color.tak,rpm.min,rpm.max,at, timepoints, at.prediction, DarkLight, day.border)  
ng.plot.ObsPred.edQTL(gn, log2rpm, log2rpm.prediction.kos, log2rpm.prediction.tak, des, color.bg,color.edQTL,rpm.min,rpm.max,at, timepoints, at.prediction, DarkLight, day.border)  
dev.off()

#Make Sup. fig. 5
genes_with_Markers = colnames(q.value)[colSums(q.value<FDR)>0]
log2rpm.prediction.diff = abs(log2rpm.prediction.kos - log2rpm.prediction.tak  )
hours = unique(at.prediction$hour)
gnl_high = colnames(log2rpm)
ex.hours.mat.kos = matrix(0, ncol = length(hours), nrow = length(gnl_high))
rownames(ex.hours.mat.kos) = gnl_high
colnames(ex.hours.mat.tak) = hours
ex.hours.mat.tak = ex.hours.mat.kos
for(i in 1:length(gnl_high)){
  gn = gnl_high[i]
  ex_hour.kos = rep(0, length(hours))
  names(ex_hour.kos) = hours
  ex_hour.tak = ex_hour.kos
  for(h in 1:length(hours)){
    ex_hour.kos[h] = median(log2rpm.prediction.kos[is.element(at.prediction$hour, hours[h]),gn])
    ex_hour.tak[h] = median(log2rpm.prediction.tak[is.element(at.prediction$hour, hours[h]),gn])
  }
  ex.hours.mat.kos[i,] = ex_hour.kos
  ex.hours.mat.tak[i,] = ex_hour.tak
  cat(sprintf("%s / %s\n", i, length(gnl_high)))
}
ex.hours.sd.kos = apply(ex.hours.mat.kos,1,FUN = sd)
ex.hours.sd.tak = apply(ex.hours.mat.tak,1,FUN = sd)

ages = list()
for(i in 3:14){
  ages = c(ages, list(c(10*i, 10*i+9)))
}
ex.ages.mat.kos = matrix(0, ncol = length(ages), nrow = length(gnl_high))
rownames(ex.ages.mat.kos) = gnl_high
colnames(ex.ages.mat.kos) = seq(3,14)
ex.ages.mat.tak = ex.ages.mat.kos
for(i in 1:length(gnl_high)){
  gn = gnl_high[i]
  ex_age.kos = rep(0, length(ages))
  names(ex_age.kos) = seq(3,14)
  ex_age.tak = ex_age.kos
  for(h in 1:length(ages)){
    ex_age.kos[h] = median(log2rpm.prediction.kos[((at.FIT.kos$age>=ages[[h]][1])&(at.FIT.kos$age<=ages[[h]][2])),gn])
    ex_age.tak[h] = median(log2rpm.prediction.tak[((at.FIT.tak$age>=ages[[h]][1])&(at.FIT.tak$age<=ages[[h]][2])),gn])
  }
  ex.ages.mat.kos[i,] = ex_age.kos
  ex.ages.mat.tak[i,] = ex_age.tak
  cat(sprintf("%s / %s\n", i, length(gnl_high)))
}
ex.ages.sd.kos = apply(ex.ages.mat.kos,1,FUN = function(x){return(sd(x[!is.na(x)]))})
ex.ages.sd.tak = apply(ex.ages.mat.tak,1,FUN = function(x){return(sd(x[!is.na(x)]))})

pdf("Figures/supfig5.pdf")
range = c(0, 5)
gn = "Os01g0537250"
plot(ex.hours.sd.kos, ex.hours.sd.tak, pch = 16, col = "gray", ylim = range, xlim = range)
points(ex.hours.sd.kos[genes_with_Markers], ex.hours.sd.tak[genes_with_Markers], pch = 16, col = "magenta")
points(ex.hours.sd.kos[c(gn,"Os03g0812500")], ex.hours.sd.tak[c(gn,"Os03g0812500")],  cex = 1.5,pch = 16, col = "orange")

plot(ex.ages.sd.kos, ex.ages.sd.tak, pch = 16, col = "gray", ylim = range, xlim = range)
points(ex.ages.sd.kos[genes_with_Markers], ex.ages.sd.tak[genes_with_Markers], pch = 16, col = "magenta")
points(ex.ages.sd.kos[c(gn,"Os03g0812500")], ex.ages.sd.tak[c(gn,"Os03g0812500")], cex = 1.5,pch = 16, col = "orange")

layout(matrix(1:2,nrow =2))
gn = "Os01g0537250"
plot(ex.hours.mat.kos[gn,],type = "n", ylim = c(-3,10))
points(ex.hours.mat.kos[gn,], col = "blue", pch = 16)
points(ex.hours.mat.tak[gn,], col = "red", pch = 16)
gn = "Os03g0812500"
plot(ex.hours.mat.kos[gn,],type = "n", ylim = c(-3,10))
points(ex.hours.mat.kos[gn,], col = "blue", pch = 16)
points(ex.hours.mat.tak[gn,], col = "red", pch = 16)

gn = "Os01g0537250"
plot(ex.ages.mat.kos[gn,],type = "n", ylim = c(-3,10))
points(ex.ages.mat.kos[gn,], col = "blue", pch = 16)
points(ex.ages.mat.tak[gn,], col = "red", pch = 16)
gn = "Os03g0812500"
plot(ex.ages.mat.kos[gn,],type = "n", ylim = c(-3,10))
points(ex.ages.mat.kos[gn,], col = "blue", pch = 16)
points(ex.ages.mat.tak[gn,], col = "red", pch = 16)
dev.off()



##################Make Sup fig6
source("scripts/scaled_age_2015.R")

#####Load timepoint and LightDark condition
load("Os2015/DarkLight_Takatsuki_2015")
load("Os2015/Sampling_timepoints_2015")

#Caluculate the border of days
remove_hour = function(x){
  return(sprintf("%s-%s-%s",x[1],x[2],x[3]))
}
day =unlist(lapply(strsplit(names(timepoints), "-"), FUN = remove_hour))
day.border = 0
pre = day[1]
first.day = TRUE
for(i in 1:length(day)){
  if(pre != day[i] && !first.day){
    day.border = c(day.border, i - 0.5)
    first.day = TRUE
    pre = day[i]
  } else if(pre != day[i]){
    first.day = FALSE
    pre = day[i]
    
  }
}
day.border = c(day.border,length(day))

####Prepare attribute for prediction of Koshihikari and Takanari expression dinamics at all timepoints.
# Caluculate age for all timepoints
attribute = "Os2015/Os2015_FIT_input/SampleAttribute_all"
load("Os2015/Os2015_FIT_input/log2rpm_all")
fn.timepoints = "Os2015/Sampling_timepoints_2015"
load(attribute)
at = at[rownames(log2rpm),]

load("Os2015/Os2015_FIT_output/kos_BG_model")
models.rnaseq.kos = models.rnaseq

load("Os2015/Os2015_FIT_output/tak_BG_model")
models.rnaseq.tak = models.rnaseq

# Prep. timepoints for caluculating pseudoAge
load(fn.timepoints)
remove_hour = function(x){
  return(sprintf("%s-%s-%s",x[1],x[2],x[3]))
}
tmp = unlist(lapply(strsplit(names(timepoints),"-"), FUN = remove_hour))
names(tmp) = names(timepoints)
timepoints= tmp

# Prep. Date for caluculating pseudoAge
SeedSowingDateList <- c("2015-04-16", "2015-04-30", "2015-05-14", "2015-05-28")
TransPlantDateList <- c("2015-05-14", "2015-05-21", "2015-06-04", "2015-06-18")
SamplingDate <- timepoints

# Summarize observed points
age = as.numeric(SamplingDate)
names(age) = names(SamplingDate)
SamplingPoints = cbind(age,age,age,age)
colnames(SamplingPoints) = 1:4
for(i in 1:4){
  tmp = at[at$TransPlantSet==i,]
  tmp = unique(sprintf("%04d-%02d-%02d-%02d", tmp$year, tmp$month, tmp$day, tmp$hour))
  SamplingPoints[tmp,i] = 1
  range = which(!is.na(SamplingPoints[,i]))
  SamplingPoints[min(range):max(range),i] = 1
}


# Predict expression dynamics of each genotype
gn = "Os01g0537250"
models.rnaseq.kos.gn = list(models.rnaseq.kos[[grep(gn, names(models.rnaseq))]])
models.rnaseq.tak.gn = list(models.rnaseq.tak[[grep(gn, names(models.rnaseq))]])

lineNames = unique(at$LineName)
lineNames = lineNames[!is.element(lineNames, lineNames[grep("HP", lineNames)])]
predictions = rep(list(NULL), length(lineNames))
names(predictions) = lineNames
log2rpm.prediction.list = list("kos" = predictions, "tak" = predictions)
for(lineName in lineNames){
  cat(sprintf("%s\n", lineName))
  age = as.numeric(SamplingDate)
  names(age) = names(SamplingDate)
  age = cbind(age,age,age,age)
  colnames(age) = 1:4
  for(i in 1:length(SamplingDate)){
    for(TS in 1:4){
      age[i,TS] <- pesudoAge(c(SamplingDate[i],lineName,TS))
    }
  }
  age[is.na(SamplingPoints)] = NA
  
  # Prepare attribute with all possible ages 
  at.prediction = list()
  for(n in 1 : nrow(at)){
    tmp = as.matrix(at[n,])
    # rownames(tmp) = NULL
    # tmp = as.vector(tmp)
    for(i in 1:4){
      # 8 #year
      # 9 #month
      # 10 #day
      # 11 #hour
      # 23 #age
      tmp[7] = i
      tmp[21] = age[sprintf("%04d-%02d-%02d-%02d",as.integer(tmp[8]), as.integer(tmp[9]), as.integer(tmp[10]), as.integer(tmp[11])),i]
      if(!is.na(tmp[21])){
        at.prediction = c(at.prediction,list(as.numeric(tmp[c(1,7:11,21,26:27)])))
      }
    }
  }
  at.prediction = unique(at.prediction)
  tmp = NULL
  for(i in 1: length(at.prediction)){
    tmp= rbind(tmp,at.prediction[[i]])
  }
  colnames(tmp) = colnames(at)[c(1,7:11,21,26:27)]
  at.FIT = data.frame(tmp, stringsAsFactors = F)
  at.FIT = at.FIT[order(at.FIT$time),]
  save(at.FIT, file = "Os2015/Os2015_FIT_input/attribute_for_prediction")
  
  #####Parameters##########
  attribute = "Os2015/Os2015_FIT_input/SampleAttribute_all"
  weather = "Os2015/Os2015_FIT_input/wtdata_2015_Takatsuki" #NA Removed

  #Load attribute information
  fn = sprintf("%s",attribute)
  prediction.attribute  <- FIT::load.attribute(fn, 'at.FIT')
  
  #Load wether information
  prediction.weather    <- FIT::load.weather(weather, 'wt', c('temperature', 'radiation'))
  
  # Koshihikari model
  prediction <- FIT::predict(models.rnaseq.kos.gn, prediction.attribute, prediction.weather)
  log2rpm.prediction.kos = t(matrix(unlist(prediction),byrow = T, nrow = 1))
  
  
  # Takanaei model
  prediction <- FIT::predict(models.rnaseq.tak.gn, prediction.attribute, prediction.weather)
  log2rpm.prediction.tak = t(matrix(unlist(prediction),byrow = T, nrow = 1))
  
  # Save prediction results
  log2rpm.prediction.list$kos[[lineName]]  = log2rpm.prediction.kos
  log2rpm.prediction.list$tak[[lineName]]  = log2rpm.prediction.tak
}
save(log2rpm.prediction.list, file = sprintf("Os2015/Os2015_FIT_output/FITresult_kos_gene.id.list_%s_prediction", gn))

rpm.max = max(unlist(log2rpm.prediction.list))+1
rpm.min = min(unlist(log2rpm.prediction.list))
lineNames = lineNames[!is.element(lineNames,c("Koshihikari","Takanari"))]

pdf(sprintf("Figures/supfig6_%s.pdf", gn))
load("Os2015/Sampling_timepoints_2015")
at.time = timepoints[sprintf("%04d-%02d-%02d-%02d", at.FIT$year, at.FIT$month, at.FIT$day, at.FIT$hour)]

for(n in 1:4){
  layout(matrix(c(1,2,3), nrow = 3))
  plot(at.time, log2rpm.prediction.list$kos[[1]], type = "n", ylim = c(rpm.min, rpm.max), xaxt = "n", main = sprintf("Transplant set %s",n))
  for(i in 1:length(DarkLight)){
    polygon(DarkLight[[i]],c(rpm.max-0.5,rpm.max-0.5,rpm.max,rpm.max),col = "gray", border = NA)
  }  
  abline(v = day.border, lty = "dashed", col = "gray")
  for(i in 1:length(lineNames)){
    lines(at.time[at.FIT$TransPlantSet==n], log2rpm.prediction.list$kos[[i]][at.FIT$TransPlantSet==n], col = colnames2rgb("cyan", 100))
  }
  lines(at.time[at.FIT$TransPlantSet==n], log2rpm.prediction.list$kos$Koshihikari[at.FIT$TransPlantSet==n], col = "blue")
  
  plot(at.time,log2rpm.prediction.list$tak[[1]], type = "n", ylim = c(rpm.min, rpm.max), xaxt = "n", main = sprintf("Transplant set %s",n))
  for(i in 1:length(DarkLight)){
    polygon(DarkLight[[i]],c(rpm.max-0.5,rpm.max-0.5,rpm.max,rpm.max),col = "gray", border = NA)
  }  
  abline(v = day.border, lty = "dashed", col = "gray")
  for(i in 1:length(lineNames)){
    lines(at.time[at.FIT$TransPlantSet==n], log2rpm.prediction.list$tak[[i]][at.FIT$TransPlantSet==n], col = colnames2rgb("pink",100))
  }
  lines(at.time[at.FIT$TransPlantSet==n], log2rpm.prediction.list$tak$Takanari[at.FIT$TransPlantSet==n], col = "red")
  
  plot(at.time,log2rpm.prediction.list$tak[[1]], type = "n", ylim = c(rpm.min, rpm.max), xaxt = "n", main = sprintf("Transplant set %s",n))
}
dev.off()

############ Make Sup Fig.4
library("fields")
requireNamespace("FIT")
source("scripts/colname2rgb.R")

blues = c("blue","Royalblue", "skyblue", "cyan") 
reds = c("red","red3","pink1","magenta")


ng.plot.ObsPred <- function(gn, ex, ks, np, des, col,col2,min,max, at, timepoints, at.prediction,DarkLight, day.border, title){
  at.time = timepoints[sprintf("%04d-%02d-%02d-%02d", at$year, at$month, at$day, at$hour)]
  plot(at.time,ex[,gn], ylim=c(min,max),
       m=sprintf("%s : %s : %s\n%s", gn, des[gn,3], des[gn,4], title),
       sub=des[gn,2], type ="n")
  for(i in 1:length(DarkLight)){
    polygon(DarkLight[[i]],c(max-1,max-1,max,max),col = "gray", border = NA)
  }  
  abline(v = day.border, lty = "dashed", col = "gray")
  for(c in 4:1){
    lines(timepoints[sprintf("%04d-%02d-%02d-%02d", at.prediction$year, at.prediction$month, at.prediction$day, at.prediction$hour)[at.prediction$TransPlantSet==c]], np[at.prediction$TransPlantSet==c,gn], col=reds[c])
    lines(timepoints[sprintf("%04d-%02d-%02d-%02d", at.prediction$year, at.prediction$month, at.prediction$day, at.prediction$hour)[at.prediction$TransPlantSet==c]], ks[at.prediction$TransPlantSet==c,gn], col=blues[c])
  }
  points(at.time,ex[,gn], col=col, pch = 16)
  points(at.time,ex[,gn], col=col2, pch = 16)
  pch = col
  pch[col2=="#FFFFFF00"] = ""
  pch[col2!="#FFFFFF00"] = 25
  pch = as.numeric(pch)
  points(at.time,ex[,gn]+2,pch =pch, bg = "black", cex = 0.8)
  
}
# load data --------------
# Load FIT result (observed)
load("Os2015/Os2015_FIT_output/kos_BG_model_prediction")
load("Os2015/Os2015_FIT_output/tak_BG_model_prediction")
load("Os2015/Os2015_FIT_input/SampleAttribute_all")

# load expression data table
load("Os2015/Os2015_FIT_input/log2rpm_all")

#Caluculate residial errors of all attribute
at = at[grep("HP", at$LineName, invert = T),]
log2rpm.all = log2rpm.all[is.element(rownames(log2rpm.all),rownames(at)),]
at = at[rownames(log2rpm.all),]
log2rpm.all = log2rpm.all[rownames(at),]
log2rpm = log2rpm.all
log2rpm.prediction.kos = log2rpm.prediction.kos[rownames(log2rpm),]
log2rpm.prediction.tak = log2rpm.prediction.tak[rownames(log2rpm),]
#Kos obeserved exp - Kos or Tak predicted exp
rsd.kos <- abs(log2rpm.prediction.kos -log2rpm.all) 
#Tak obeserved exp - Kos or Tak predicted exp
rsd.tak <- abs(log2rpm.prediction.tak - log2rpm.all)

# load FIT resulta
load("Os2015/Os2015_FIT_input/attribute_for_prediction")
at.prediction = at.FIT

# genotype --------------------
lineIDkos <- unique(c("Koshihikari",at$LineName[grep("SL12",at$LineName)]))
#lineIDtak <- c(43:82,84:86)
lineIDtak <- unique(c("Takanari",at$LineName[grep("SL13",at$LineName)]))
sample.kos <- is.element(at$LineName, lineIDkos)
sample.tak <- is.element(at$LineName, lineIDtak)
sample.all <- is.element(at$LineName, c(lineIDtak, lineIDkos))
names(sample.all) = rownames(at)
names(sample.kos) = rownames(at)
names(sample.tak) = rownames(at)

#load transcript description 
load("General_input/141103-1_GeneDescription_Osa")

# load genotype matrix
gtmat <- read.delim("General_input/161117_Genotype_KosTakCSSL_HPa12.txt", header=T, as.is=T)
gt.at <- gtmat[,1:4]
gt.mat <- gtmat[,5:ncol(gtmat)] 
gt.mat[gt.mat=="A"] = 1
gt.mat[gt.mat=="B"] = 0
gt.mat.sum = colSums(gt.mat>0)
gt.mat.sum[gt.mat.sum<50] = 0
gt.mat.sum[gt.mat.sum>50] = 1

#####Load timepoint and LightDark condition
load("Os2015/DarkLight_Takatsuki_2015")
load("Os2015/Sampling_timepoints_2015")

#Caluculate the border of days
load("Os2015/Sampling_timepoints_2015")
remove_hour = function(x){
  return(sprintf("%s-%s-%s",x[1],x[2],x[3]))
}
day =unlist(lapply(strsplit(names(timepoints), "-"), FUN = remove_hour))
day.border = 0
pre = day[1]
first.day = TRUE
for(i in 1:length(day)){
  if(pre != day[i] && !first.day){
    day.border = c(day.border, i - 0.5)
    first.day = TRUE
    pre = day[i]
  } else if(pre != day[i]){
    first.day = FALSE
    pre = day[i]
    
  }
}
day.border = c(day.border,length(day))


#####Load p-value and q-value
load("Os2015/pqvalue")
FDR=0.05

#####Load gnl.cis.trans.edQTL
load("Os2015/gnl-trans-edQTL")

#######Determaine markers significantly effect gene expression
sum.rsd.gnl.multi.edQTL = matrix(0,ncol = length(gnl.multi.edQTL), nrow=2)
colnames(sum.rsd.gnl.multi.edQTL) = gnl.multi.edQTL
rownames(sum.rsd.gnl.multi.edQTL) = c("All","Most-sig")

pdf("Figures/supfig4a-c.pdf",width = 9)
rpm.min = min(log2rpm.all)
rpm.max = max(log2rpm.all)
load("Os2015/Os2015_FIT_output/kostak_prediction")
for(gn in gnl.multi.edQTL){
  gn.markers.q.value = q.value[,gn]
  names(gn.markers.q.value) = seq(1,length(gn.markers.q.value))
  gn.markers = gn.markers.q.value<FDR
  
  #Preparation of color for kos and tak
  color = rep(colnames2rgb("white",0), length(sample.kos))
  names(color) = names(sample.kos)
  color[rownames(at)[at$LineName=="Koshihikari"]] = blues[at$TransPlantSet[at$LineName=="Koshihikari"]]
  color[rownames(at)[at$LineName=="Takanari"]] = reds[at$TransPlantSet[at$LineName=="Takanari"]]
  color.changed = color
  
  #Preparation of color based on background genotype
  color.BG = color
  color.BG[sample.kos] = blues[at$TransPlantSet[sample.kos]]
  color.BG[sample.tak] = reds[at$TransPlantSet[sample.tak]]
  color.BG.changed= color.BG
  
  # detect peaks 
  peak.pos = NULL
  before.negative = 1
  for(z in 1:length(gn.markers)){
    if(gn.markers[z]==TRUE && before.negative == 1){
      sp = z
      before.negative = 0
    } 
    if(gn.markers[z]==FALSE && before.negative == 0){
      ep = z-1
      c(sp, ep)
      peak.pos = c(peak.pos, list(c(sp, ep)))
      before.negative = 1
    }
    if(z == length(gn.markers) && before.negative == 0){
      ep = z
      peak.pos = c(peak.pos, list(c(sp, ep)))
    }
    
  }
  #detect most siginificant marker in each peak
  peak.pos.sig = NULL
  for(i in 1:length(peak.pos)){
    sp = peak.pos[[i]][1]
    ep = peak.pos[[i]][2]
    if(sp < ep){
      q.value.tmp = q.value[sp:ep,gn]
      q.value.min = min(q.value.tmp)
      peak.pos.sig = c(peak.pos.sig,(sp:ep)[q.value.tmp==q.value.min])   
    } else {
      peak.pos.sig = c(peak.pos.sig,sp)
    }
  }
  
  color.markers.each.peak = colnames2rgb(color.BG,20)
  color.markers.each.peak.changed = color.markers.each.peak
  color.markers.each.peak.changed =rep(colnames2rgb("white",0),length(color.markers.each.peak.changed))
  names(color.markers.each.peak.changed) = names(sample.kos)
  gt.mat.gn.markers = gt.mat[peak.pos.sig,]
  hit.line = colSums(gt.mat.gn.markers != matrix(rep(gt.mat.sum, nrow(gt.mat.gn.markers)), byrow = T, ncol = ncol(gt.mat.gn.markers)))>0
  hit.line = names(hit.line)[hit.line]
  hit.line = rownames(at)[is.element(at$LineName, hit.line)]
  sample.kos2 = sample.kos
  sample.tak2 = sample.tak
  sample.kos2[names(sample.kos)[sample.kos][is.element(names(sample.kos)[sample.kos], hit.line)]] = FALSE
  sample.tak2[names(sample.kos)[sample.kos][is.element(names(sample.kos)[sample.kos], hit.line)]] = TRUE
  sample.tak2[names(sample.tak)[sample.tak][is.element(names(sample.tak)[sample.tak], hit.line)]] = FALSE
  sample.kos2[names(sample.tak)[sample.tak][is.element(names(sample.tak)[sample.tak], hit.line)]] = TRUE
  sum.rsd.gnl.multi.edQTL[1,gn] = sum(rsd.kos[sample.kos2,gn])+sum(rsd.tak[sample.tak2,gn])
  color.markers.each.peak.changed[hit.line[is.element(hit.line, rownames(at)[sample.kos])]] = reds[at[hit.line[is.element(hit.line, rownames(at)[sample.kos])],"TransPlantSet"]]
  color.markers.each.peak.changed[hit.line[is.element(hit.line, rownames(at)[sample.tak])]] = blues[at[hit.line[is.element(hit.line, rownames(at)[sample.tak])],"TransPlantSet"]]
  
  #Select the prediction models based on most significant peak  
  #Detect most siginificant markers
  peak.pos.sig = NULL
  sp = 1
  ep = 141
  if(sp < ep){
    q.value.tmp = q.value[sp:ep,gn]
    q.value.min = min(q.value.tmp)
    peak.pos.sig = c(peak.pos.sig,(sp:ep)[q.value.tmp==q.value.min])   
  } else {
    peak.pos.sig = c(peak.pos.sig,sp)
  }
  
  color.markers = colnames2rgb(color.BG,20)
  color.markers.changed =rep(colnames2rgb("white",0),length(color.markers))
  names(color.markers.changed) = names(sample.kos)
  gt.mat.gn.markers = gt.mat[peak.pos.sig,]
  hit.line = colSums(gt.mat.gn.markers != matrix(rep(gt.mat.sum, nrow(gt.mat.gn.markers)), byrow = T, ncol = ncol(gt.mat.gn.markers)))>0
  hit.line = names(hit.line)[hit.line]
  hit.line = rownames(at)[is.element(at$LineName, hit.line)]
  sample.kos2 = sample.kos
  sample.tak2 = sample.tak
  sample.kos2[names(sample.kos)[sample.kos][is.element(names(sample.kos)[sample.kos], hit.line)]] = FALSE
  sample.tak2[names(sample.kos)[sample.kos][is.element(names(sample.kos)[sample.kos], hit.line)]] = TRUE
  sample.tak2[names(sample.tak)[sample.tak][is.element(names(sample.tak)[sample.tak], hit.line)]] = FALSE
  sample.kos2[names(sample.tak)[sample.tak][is.element(names(sample.tak)[sample.tak], hit.line)]] = TRUE
  sum.rsd.gnl.multi.edQTL[2,gn] = sum(rsd.kos[sample.kos2,gn])+sum(rsd.tak[sample.tak2,gn])
  color.markers.changed[hit.line[is.element(hit.line, rownames(at)[sample.kos])]] = reds[at[hit.line[is.element(hit.line, rownames(at)[sample.kos])],"TransPlantSet"]]
  color.markers.changed[hit.line[is.element(hit.line, rownames(at)[sample.tak])]] = blues[at[hit.line[is.element(hit.line, rownames(at)[sample.tak])],"TransPlantSet"]]
  
  layout(matrix(c(1,2,3,4,5,6), byrow = T, ncol = 2))
  ng.plot.ObsPred(gn, log2rpm.all, log2rpm.prediction.kos, log2rpm.prediction.tak, des, color, color.changed,rpm.min,rpm.max,at, timepoints, at.prediction, DarkLight, day.border,"kos_tak")
  ng.plot.ObsPred(gn, log2rpm.all, log2rpm.prediction.kos, log2rpm.prediction.tak, des, color.BG, color.BG.changed,rpm.min,rpm.max,at, timepoints, at.prediction, DarkLight, day.border,"Background")
  ng.plot.ObsPred(gn, log2rpm.all, log2rpm.prediction.kos, log2rpm.prediction.tak, des, color.markers.each.peak,color.markers.each.peak.changed, rpm.min,rpm.max,at, timepoints, at.prediction, DarkLight, day.border,sprintf("%s_Most significant each peak", sum.rsd.gnl.multi.edQTL[1,gn]))
  ng.plot.ObsPred(gn, log2rpm.all, log2rpm.prediction.kos, log2rpm.prediction.tak, des, color.markers,color.markers.changed, rpm.min,rpm.max,at, timepoints, at.prediction, DarkLight, day.border,sprintf("%s_Most significant peak", sum.rsd.gnl.multi.edQTL[2,gn]))
  col.q.value = rep("gray",nrow(gt.mat))
  col.q.value[q.value[,gn]<FDR] = "red"
  range = range(-log10(p.value[,gn]))
  if(range[2]<6){
    range[2]=6
  }
  plot(-log10(q.value[,gn]),ylim = range, xlab = "marker", pch = 16)
  lines(-log10(q.value[,gn]), ylim = range)
  points(-log10(q.value[,gn]), col=col.q.value, ylim = range, pch =16)
  abline(h = -log10(FDR))
  abline(v=c(0,cumsum(table(gt.at$chr.)))+0.5, col="#666666", lty="dotted")
  plot(-log10(q.value[,gn]),ylim = range, xlab = "marker", pch = 16)
}
dev.off()

#Make sup fig.4d
pdf("Figures/supfig4d.pdf", width = 21)
plot(sum.rsd.gnl.multi.edQTL["Most-sig",order(colnames(sum.rsd.gnl.multi.edQTL))]-sum.rsd.gnl.multi.edQTL["All",order(colnames(sum.rsd.gnl.multi.edQTL))], pch = 16)
abline(h = 0, col = "red")
dev.off()

############################Make Fig.3a
#load sample attribute 
at = read.table("Os2016/Os2016_input/SampleAttribute_2016_Kizu.txt", header = T, fill = T)

#Parse timepoints
timepoints = unique(sprintf("%s:%s:%s:%s:0", at$year, at$month, at$day, at$hour))

# Load weather data
#2015
load("Os2015/Os2015_FIT_input/wtdata_2015_Takatsuki")
wt.taka = wt[((is.element(wt$month, c(8)))) & (is.element(wt$min, seq(0,50,10))),]

#2016
load("Os2016/Os2016_FIT_input/wtdata_2016_Kizugawa")
wt = wt[((is.element(wt$month, c(8))))& (is.element(wt$min, seq(0,50,10))),]
time.sum = unique(sprintf("%s:%s:%s:%s:%s", wt$year, wt$month, wt$day, wt$hour, wt$min))
names(time.sum) = seq(1,length(time.sum))

#Caluculate month border and sampling time 
table.accum = function(x){
  tmp = table(x)
  for(i in 2:length(tmp)){
    tmp[i] = tmp[i-1] + tmp[i]
  }
  return(tmp)
}
sampling.time = names(time.sum)[is.element(time.sum, timepoints)]

# Make list of samplin day set
tmp = strsplit(unique(sprintf("%s:%s",at$month, at$day)),":")
sampling.day.set = list()
for(i in 1:(length(tmp)/2)){
  a = tmp[[2*i-1]]
  b = tmp[[2*i]]
  sampling.day.set = c(sampling.day.set, list(c(a,b[2])))
}

pdf("Figures/fig3a.pdf")
sampling.time = as.numeric(sampling.time)
layout(matrix(c(1,2), ncol = 1, byrow = T))
plot(wt.taka$temperature[sampling.time], type = "n", main = "Temperature", xaxt ="n")
lines(1:12,wt$temperature[sampling.time][1:12], col = "red")
lines(1:12,wt.taka$temperature[sampling.time][1:12])
lines(13:24,wt$temperature[sampling.time][13:24], col = "red")
lines(13:24,wt.taka$temperature[sampling.time][13:24])
lines(25:36,wt$temperature[sampling.time][25:36], col = "red")
lines(25:36,wt.taka$temperature[sampling.time][25:36])
abline(v = c(12.5,24.5,36), col = "gray", lty = "dashed")


plot(wt$radiation[sampling.time], type = "n", main = "Radiation", xaxt ="n")
lines(1:12,wt$radiation[sampling.time][1:12], col = "red")
lines(1:12,wt.taka$radiation[sampling.time][1:12])
lines(13:24,wt$radiation[sampling.time][13:24], col = "red")
lines(13:24,wt.taka$radiation[sampling.time][13:24])
lines(25:36,wt$radiation[sampling.time][25:36], col = "red")
lines(25:36,wt.taka$radiation[sampling.time][25:36])
abline(v = c(12.5,24.5,36), col = "gray", lty = "dashed")
dev.off()

###########Parameters##############
load("Os2016/Os2016_input/wtdata_2016_Nara")
load("Os2016/Sampling_timepoints_2016")
wt.time = sprintf("%04d-%02d-%02d-%02d-%02d", wt$year, wt$month, wt$day, wt$hour, wt$min)
rownames(wt) = wt.time
radiation = wt[sprintf("%s-00", names(timepoints)),]$radiation
dark = radiation<0.3 
sp = 0
ep = 0
is.dark = FALSE
dark.time = list()
for(i in 1:length(dark)){
  if(dark[i]==TRUE && !is.dark){
    is.dark = TRUE
    sp = i
  } else if(dark[i] == FALSE && is.dark){
    is.dark = FALSE
    ep = i - 1
    dark.time = c(dark.time, list(c(sp, ep)))
  }
}
if(is.dark){
  dark.time = c(dark.time, list(c(sp, i)))
}

DarkLight = list()
for(i in 1:length(dark.time)){
  dark = dark.time[[i]]
  sp = dark[1]
  ep = dark[2]
  DarkLight = c(DarkLight,list(c(sp,ep,ep,sp)))
}
save(DarkLight, file = "Os2016/DarkLight_Nara_2016")

#################### Prepare input data of 2016 for FIT
#load transcript description --------------------
load("General_input/141103-1_GeneDescription_Osa")

#load sample attribute 
at = read.table("Os2016/Os2016_input/SampleAttribute_2016_Kizu.txt", header = T, stringsAsFactors = F, fill = T)
rownames(at) = at[,1]
at.all = at[colnames(log2rpm.all),]

# load expression data table 
load("Os2016/rawcnt-filtered_Osa_2016")
log2rpm = log2(t(t(rawcnt)/colSums(rawcnt)*10^6+0.1))
log2rpm = log2rpm[,is.element(colnames(log2rpm), rownames(at))]
log2rpm.all = log2rpm

# make attribute matrix -----------------
SeedSowingDateList <- c("2016-04-21", "2016-04-21", "2016-05-19", "2016-05-19")
TransPlantDateList <- c("2016-05-16", "2016-05-16", "2016-06-08", "2016-06-08")
SeedSowingDate <- SeedSowingDateList[at.all[,"TransPlantSet"]]
TransPlantDate <- TransPlantDateList[at.all[,"TransPlantSet"]]
SamplingDate <- sprintf("%s-%02d-%02d", at.all$year, at.all$month, at.all$day)
DayAfterSowing <- as.numeric(as.Date(SamplingDate) - as.Date(SeedSowingDate))
DayAfterTransplant <- as.numeric(as.Date(SamplingDate) - as.Date(TransPlantDate))
age <- DayAfterSowing

tt <- with(at.all, sprintf("%s-%02d-%02d %02d:00:00", year, month, day, hour))
st <- strptime(tt, "%Y-%m-%d %H:%M:%S")
st.ori <- strptime("2016-01-01 00:00:00", "%Y-%m-%d %H:%M:%S")
time <- as.numeric(st)/60 - as.numeric(st.ori)/60 +1

type <- rep(0, nrow(at.all))

at.all <- cbind(time, at.all, age, DayAfterSowing, DayAfterTransplant, SeedSowingDate, TransPlantDate, type)

min = matrix(0,nrow = nrow(at.all), ncol=1)
at.all = cbind(at.all,min)
at.all = at.all[!is.element(colnames(at.all),c("KobayashiComment","ErrorConditionChange"))]

#calculate sd of counts of each gene at each timepoint and convert to weights.
# prep. rpm, at for Kos and Tak -----------------
log2rpm.all = t(log2rpm)
save(log2rpm.all, file = "Os2016/Os2016_FIT_input/log2rpm_all")
at <- at.all
save(at, file="Os2016/Os2016_FIT_input//SampleAttribute_all")


##################### Evaluation of prediction accuracy under different environments from where samples for model construction were collected 
################
### function ###
################
library("fields")
requireNamespace("FIT")
library("plyr")
source("scripts/ng.Colors.R")
source("scripts/ng.BHFDR.R")
source("scripts/colname2rgb.R")
source("scripts/scaled_age_2016.R")

blues = c("blue","Royalblue", "skyblue", "cyan") 
reds = c("red","red3","pink1","magenta")

##############
### script ###
##############
# load data --------------
# load expression data table
load("Os2016/Os2016_FIT_input/log2rpm_all")
gene.id = colnames(log2rpm.all)
#load transcript description 
load("General_input/141103-1_GeneDescription_Osa")

# load sample attribute 
load("Os2016/Os2016_FIT_input/SampleAttribute_all")
at = at[sprintf("%s",sort(at$ID)),]
at = at[at$LineName!="Unpredictable",]

# Load timepoints 
load("Os2016/Sampling_timepoints_2016")

#Caluculate the border of days
remove_hour = function(x){
  return(sprintf("%s-%s-%s",x[1],x[2],x[3]))
}
day =unlist(lapply(strsplit(names(timepoints), "-"), FUN = remove_hour))
day.border = 0
pre = day[1]
first.day = TRUE
for(i in 1:length(day)){
  if(pre != day[i] && !first.day){
    day.border = c(day.border, i - 0.5)
    first.day = TRUE
    pre = day[i]
  } else if(pre != day[i]){
    first.day = FALSE
    pre = day[i]
    
  }
}
day.border = c(day.border,length(day))

# Load DarkLight
load("Os2016/DarkLight_Nara_2016")

# Load prediction model matrix
load("Os2015/prediction_model_mat")

# Load p-value and q-value
load("Os2015/pqvalue")
genes_with_edQTL = colnames(q.value)[colSums(q.value<FDR)>0]

# Load list of genes with refference
load("General_input//ref_list")
genes_with_edQTL_ref = genes_with_edQTL[is.element(genes_with_edQTL, ref.list)]

##############Prepare attribute for prediction of expression dynamics at all timepoints.
# Caluculate age for all timepoints
load("Os2016/Os2016_FIT_input/SampleAttribute_all")

# Load FIT predictive models
load("Os2015/Os2015_FIT_output/kos_BG_model")
models.rnaseq.kos = models.rnaseq
load("Os2015/Os2015_FIT_output/tak_BG_model")
models.rnaseq.tak = models.rnaseq

# Prep. timepoints for caluculating pseudoAge
load("Os2016/Sampling_timepoints_2016")
remove_hour = function(x){
  return(sprintf("%s-%s-%s",x[1],x[2],x[3]))
}
tmp = unlist(lapply(strsplit(names(timepoints),"-"), FUN = remove_hour))
names(tmp) = names(timepoints)
timepoints= tmp

# Prep. Date for caluculating pseudoAge
SeedSowingDateList <- c("2016-04-21","2016-05-19")
TransPlantDateList <- c("2016-05-16","2016-06-08")
SamplingDate <- timepoints

# Summarize observed points
age = as.numeric(SamplingDate)
names(age) = names(SamplingDate)
SamplingPoints = cbind(age,age)
colnames(SamplingPoints) = 1:2
for(i in 1:2){
  tmp = at[at$TransPlantSet==i,]
  tmp = unique(sprintf("%04d-%02d-%02d-%02d", tmp$year, tmp$month, tmp$day, tmp$hour))
  SamplingPoints[tmp,i] = 1
  range = which(!is.na(SamplingPoints[,i]))
  SamplingPoints[min(range):max(range),i] = 1
}


#### Predict expression dynamics of each genotype
# Parse prediction model
models.rnaseq.kos.gnl = rep(list(list()), length(genes_with_edQTL))
names(models.rnaseq.kos.gnl) = genes_with_edQTL
models.rnaseq.tak.gnl = models.rnaseq.kos.gnl
for(gn in genes_with_edQTL){
  models.rnaseq.kos.gnl[[gn]] = models.rnaseq.kos[[grep(gn, names(models.rnaseq))]]
  models.rnaseq.tak.gnl[[gn]] = models.rnaseq.tak[[grep(gn, names(models.rnaseq))]]
}

# Prep attribute of each genotype for predction
lineNames = colnames(prediction.model.mat)
lineNames = lineNames[!is.element(lineNames, lineNames[grep("HP", lineNames)])]
tmp = read.table("Os2016/Os2016_input/2016CSSLs_headingDate.txt", header = T)
lineNames = lineNames[is.element(lineNames, rownames(tmp))]
predictions = rep(list(NULL), length(lineNames))
names(predictions) = lineNames
log2rpm.prediction.list = list("kos" = predictions, "tak" = predictions)
for(lineName in lineNames){
  cat(sprintf("%s\n", lineName))
  age = as.numeric(SamplingDate)
  names(age) = names(SamplingDate)
  age = cbind(age,age)
  colnames(age) = 1:2
  for(i in 1:length(SamplingDate)){
    for(TS in 1:2){
      age[i,TS] <- pesudoAge(c(SamplingDate[i],lineName,TS))
    }
  }
  age[is.na(SamplingPoints)] = NA
  
  # Prepare attribute with all possible ages 
  at.prediction = list()
  for(n in 1 : nrow(at)){
    tmp = as.matrix(at[n,])
    for(i in c(1,2)){
      tmp[3] = i
      tmp[25] = age[sprintf("%04d-%02d-%02d-%02d",as.integer(tmp[4]), as.integer(tmp[5]), as.integer(tmp[6]), as.integer(tmp[7])),i]
      if(!is.na(tmp[25])){
        at.prediction = c(at.prediction,list(as.numeric(tmp[c(1,3,4:7,25,30:31)])))
      }
    }
  }
  at.prediction = unique(at.prediction)
  tmp = NULL
  for(i in 1: length(at.prediction)){
    tmp= rbind(tmp,at.prediction[[i]])
  }
  colnames(tmp) = colnames(at)[c(1,3,4:7,25,30:31)]
  at.FIT = data.frame(tmp, stringsAsFactors = F)
  at.FIT = at.FIT[order(at.FIT$time),]
  at.FIT.kos = at.FIT
  save(at.FIT, file = "Os2016/Os2016_FIT_input/attribute_for_prediction")
  
  #####Parameters##########
  attribute = "Os2016/Os2016_FIT_input/attribute_for_prediction"
  weather = "Os2016/Os2016_FIT_input/wtdata_2016_Kizugawa" #NA Removed
  
  #Load attribute information
  fn = sprintf("%s",attribute)
  prediction.attribute  <- FIT::load.attribute(fn, 'at.FIT')
  
  #Load wether information
  prediction.weather    <- FIT::load.weather(weather, 'wt', c('temperature', 'radiation'))
  
  # Koshihikari model
  prediction <- FIT::predict(models.rnaseq.kos.gnl, prediction.attribute, prediction.weather)
  log2rpm.prediction.kos = matrix(unlist(prediction),byrow = F, nrow = nrow(at.FIT))
  colnames(log2rpm.prediction.kos) = genes_with_edQTL
  
  
  # Takanaei model
  prediction <- FIT::predict(models.rnaseq.tak.gnl, prediction.attribute, prediction.weather)
  log2rpm.prediction.tak = matrix(unlist(prediction),byrow = F, nrow = nrow(at.FIT))
  colnames(log2rpm.prediction.tak) = genes_with_edQTL
  
  # Save prediction results
  log2rpm.prediction.list$kos[[lineName]]  = log2rpm.prediction.kos
  log2rpm.prediction.list$tak[[lineName]]  = log2rpm.prediction.tak
}
save(log2rpm.prediction.list, file = sprintf("Os2016/Os2016_FIT_output/%s_prediction", "genes_with_edQTL"))

# Seacrch CSSL with edQTL of each gene
CSSLs.kos.BG = colnames(prediction.model.mat)[is.element(colnames(prediction.model.mat), c("Koshihikari", colnames(prediction.model.mat)[grep("SL12", colnames(prediction.model.mat))]))]
CSSLs.tak.BG = colnames(prediction.model.mat)[is.element(colnames(prediction.model.mat), c("Takanari", colnames(prediction.model.mat)[grep("SL13", colnames(prediction.model.mat))]))]
CSSLs.kos.BG = CSSLs.kos.BG[is.element(CSSLs.kos.BG, lineNames)]
CSSLs.tak.BG = CSSLs.tak.BG[is.element(CSSLs.tak.BG, lineNames)]
line.gene.edQTL.list = rep(list(list()),ncol(prediction.model.mat))
names(line.gene.edQTL.list) = colnames(prediction.model.mat)
for(gn in genes_with_edQTL){
  prediction.model.mat.gn = prediction.model.mat[gn,]
  kos.edQTL.gn = CSSLs.kos.BG[prediction.model.mat.gn[CSSLs.kos.BG] == "B"]
  tak.edQTL.gn = CSSLs.tak.BG[prediction.model.mat.gn[CSSLs.tak.BG] == "A"]
  line.gene.edQTL.list[[gn]] = list(kos = kos.edQTL.gn, tak = tak.edQTL.gn)
}

rpm.max = max(log2rpm.all)+1
rpm.min = min(log2rpm.all)
load("Os2016/Sampling_timepoints_2016")
at.time = timepoints[sprintf("%04d-%02d-%02d-%02d", at.FIT$year, at.FIT$month, at.FIT$day, at.FIT$hour)]
pdf(sprintf("Figures/fig3b.pdf"), height = 3.5, width = 3.5)
n= 1 
c = 1
for(gn in genes_with_edQTL){
  edQTL.line.kos = line.gene.edQTL.list[[gn]]$kos
  non.edQTL.line.kos = CSSLs.kos.BG[!is.element(CSSLs.kos.BG,edQTL.line.kos)]
  edQTL.line.tak = line.gene.edQTL.list[[gn]]$tak
  non.edQTL.line.tak = CSSLs.tak.BG[!is.element(CSSLs.tak.BG,edQTL.line.tak)]
  plot(at.time, log2rpm.prediction.list$kos[[1]][,gn], type = "n", ylim = c(rpm.min, rpm.max), xaxt = "n", main = sprintf("%s", gn) , sub = sprintf("Transplant set %s, %s, %s",n, edQTL.line.kos[1], edQTL.line.tak[1]))
  for(i in 1:length(DarkLight)){
    polygon(DarkLight[[i]],c(rpm.max-1,rpm.max-1,rpm.max,rpm.max),col = "gray", border = NA)
  }  
  abline(v = day.border, lty = "dashed", col = "gray")
  for(lineName in non.edQTL.line.kos){
    lines(at.time[at.FIT$TransPlantSet==n], log2rpm.prediction.list$kos[[lineName]][at.FIT$TransPlantSet==n,gn], col = colnames2rgb("cyan", 100))
  }
  for(lineName in non.edQTL.line.tak){
    lines(at.time[at.FIT$TransPlantSet==n], log2rpm.prediction.list$tak[[lineName]][at.FIT$TransPlantSet==n,gn], col = colnames2rgb("pink", 100))
  }
  for(lineName in edQTL.line.kos){
    lines(at.time[at.FIT$TransPlantSet==n], log2rpm.prediction.list$tak[[lineName]][at.FIT$TransPlantSet==n,gn], col = colnames2rgb("red", 100))
  }
  for(lineName in edQTL.line.tak){
    lines(at.time[at.FIT$TransPlantSet==n], log2rpm.prediction.list$kos[[lineName]][at.FIT$TransPlantSet==n,gn], col = colnames2rgb("blue", 100))
  }
  at.tmp = at[at$TransPlantSet==c,]
  points(timepoints[sprintf("%04d-%02d-%02d-%02d", at.tmp$year, at.tmp$month, at.tmp$day, at.tmp$hour)[is.element(at.tmp$LineName, CSSLs.kos.BG)]], log2rpm.all[rownames(at)[(at$TransPlantSet==c)&is.element(at$LineName, CSSLs.kos.BG)], gn], pch = 16, col = colnames2rgb("blue", 20))
  points(timepoints[sprintf("%04d-%02d-%02d-%02d", at.tmp$year, at.tmp$month, at.tmp$day, at.tmp$hour)[is.element(at.tmp$LineName, CSSLs.tak.BG)]], log2rpm.all[rownames(at)[(at$TransPlantSet==c)&is.element(at$LineName, CSSLs.tak.BG)], gn], pch = 16, col = colnames2rgb("red", 20))
  points(timepoints[sprintf("%04d-%02d-%02d-%02d", at.tmp$year, at.tmp$month, at.tmp$day, at.tmp$hour)[is.element(at.tmp$LineName, edQTL.line.tak)]], log2rpm.all[rownames(at)[(at$TransPlantSet==c)&is.element(at$LineName, edQTL.line.tak)], gn], pch = 16, col = colnames2rgb("blue", 100))
  points(timepoints[sprintf("%04d-%02d-%02d-%02d", at.tmp$year, at.tmp$month, at.tmp$day, at.tmp$hour)[is.element(at.tmp$LineName, edQTL.line.kos)]], log2rpm.all[rownames(at)[(at$TransPlantSet==c)&is.element(at$LineName, edQTL.line.kos)],gn], pch = 16, col = colnames2rgb("red", 100))
  points(timepoints[sprintf("%04d-%02d-%02d-%02d", at.tmp$year, at.tmp$month, at.tmp$day, at.tmp$hour)[is.element(at.tmp$LineName, edQTL.line.tak)]], log2rpm.all[rownames(at)[(at$TransPlantSet==c)&is.element(at$LineName, edQTL.line.tak)], gn]+2, pch = 25, col = "black", bg = "black")
  points(timepoints[sprintf("%04d-%02d-%02d-%02d", at.tmp$year, at.tmp$month, at.tmp$day, at.tmp$hour)[is.element(at.tmp$LineName, edQTL.line.kos)]], log2rpm.all[rownames(at)[(at$TransPlantSet==c)&is.element(at$LineName, edQTL.line.kos)],gn]+2, pch = 25, col = "black", bg = "black")
}
dev.off()

pdf(sprintf("Figures/Sup_fig7.pdf"), height = 3.5, width = 3.5)
c = 2
n = c
for(gn in genes_with_edQTL){
  edQTL.line.kos = line.gene.edQTL.list[[gn]]$kos
  non.edQTL.line.kos = CSSLs.kos.BG[!is.element(CSSLs.kos.BG,edQTL.line.kos)]
  edQTL.line.tak = line.gene.edQTL.list[[gn]]$tak
  non.edQTL.line.tak = CSSLs.tak.BG[!is.element(CSSLs.tak.BG,edQTL.line.tak)]
  plot(at.time, log2rpm.prediction.list$kos[[1]][,gn], type = "n", ylim = c(rpm.min, rpm.max), xaxt = "n", main = sprintf("%s", gn) , sub = sprintf("Transplant set %s, %s, %s",n, edQTL.line.kos[1], edQTL.line.tak[1]))
  for(i in 1:length(DarkLight)){
    polygon(DarkLight[[i]],c(rpm.max-1,rpm.max-1,rpm.max,rpm.max),col = "gray", border = NA)
  }  
  abline(v = day.border, lty = "dashed", col = "gray")
  for(lineName in non.edQTL.line.kos){
    lines(at.time[at.FIT$TransPlantSet==n], log2rpm.prediction.list$kos[[lineName]][at.FIT$TransPlantSet==n,gn], col = colnames2rgb("cyan", 100))
  }
  for(lineName in non.edQTL.line.tak){
    lines(at.time[at.FIT$TransPlantSet==n], log2rpm.prediction.list$tak[[lineName]][at.FIT$TransPlantSet==n,gn], col = colnames2rgb("pink", 100))
  }
  for(lineName in edQTL.line.kos){
    lines(at.time[at.FIT$TransPlantSet==n], log2rpm.prediction.list$tak[[lineName]][at.FIT$TransPlantSet==n,gn], col = colnames2rgb("red", 100))
  }
  for(lineName in edQTL.line.tak){
    lines(at.time[at.FIT$TransPlantSet==n], log2rpm.prediction.list$kos[[lineName]][at.FIT$TransPlantSet==n,gn], col = colnames2rgb("blue", 100))
  }
  at.tmp = at[at$TransPlantSet==c,]
  points(timepoints[sprintf("%04d-%02d-%02d-%02d", at.tmp$year, at.tmp$month, at.tmp$day, at.tmp$hour)[is.element(at.tmp$LineName, CSSLs.kos.BG)]], log2rpm.all[rownames(at)[(at$TransPlantSet==c)&is.element(at$LineName, CSSLs.kos.BG)], gn], pch = 16, col = colnames2rgb("blue", 20))
  points(timepoints[sprintf("%04d-%02d-%02d-%02d", at.tmp$year, at.tmp$month, at.tmp$day, at.tmp$hour)[is.element(at.tmp$LineName, CSSLs.tak.BG)]], log2rpm.all[rownames(at)[(at$TransPlantSet==c)&is.element(at$LineName, CSSLs.tak.BG)], gn], pch = 16, col = colnames2rgb("red", 20))
  points(timepoints[sprintf("%04d-%02d-%02d-%02d", at.tmp$year, at.tmp$month, at.tmp$day, at.tmp$hour)[is.element(at.tmp$LineName, edQTL.line.tak)]], log2rpm.all[rownames(at)[(at$TransPlantSet==c)&is.element(at$LineName, edQTL.line.tak)], gn], pch = 16, col = colnames2rgb("blue", 100))
  points(timepoints[sprintf("%04d-%02d-%02d-%02d", at.tmp$year, at.tmp$month, at.tmp$day, at.tmp$hour)[is.element(at.tmp$LineName, edQTL.line.kos)]], log2rpm.all[rownames(at)[(at$TransPlantSet==c)&is.element(at$LineName, edQTL.line.kos)],gn], pch = 16, col = colnames2rgb("red", 100))
  points(timepoints[sprintf("%04d-%02d-%02d-%02d", at.tmp$year, at.tmp$month, at.tmp$day, at.tmp$hour)[is.element(at.tmp$LineName, edQTL.line.tak)]], log2rpm.all[rownames(at)[(at$TransPlantSet==c)&is.element(at$LineName, edQTL.line.tak)], gn]+2, pch = 25, col = "black", bg = "black")
  points(timepoints[sprintf("%04d-%02d-%02d-%02d", at.tmp$year, at.tmp$month, at.tmp$day, at.tmp$hour)[is.element(at.tmp$LineName, edQTL.line.kos)]], log2rpm.all[rownames(at)[(at$TransPlantSet==c)&is.element(at$LineName, edQTL.line.kos)],gn]+2, pch = 25, col = "black", bg = "black")
}
dev.off()


################################# Prediction of expression at observed timepoints and genotype in 2016
#Load attribute information
prediction.attribute  <- FIT::load.attribute("Os2016/Os2016_FIT_input/SampleAttribute_all", 'at')

#Load wether information
prediction.weather    <- FIT::load.weather("Os2016/Os2016_FIT_input/wtdata_2016_Kizugawa", 'wt', c('temperature', 'radiation'))

#Load used.gene and attribute
load("Os2016/Os2016_FIT_input/log2rpm_all")
load("Os2016/Os2016_FIT_input/SampleAttribute_all")

###Load data, merge and save
load("Os2015/Os2015_FIT_output/kos_BG_model")
gnl = names(models.rnaseq)
tmp = strsplit(gnl,"\\.")
gnl = unlist(lapply(tmp, FUN = function(x){return(x[[1]])}))
prediction <- FIT::predict(models.rnaseq, prediction.attribute, prediction.weather)
log2rpm.prediction.kos = t(matrix(unlist(prediction),byrow = T, nrow = length(prediction)))
colnames(log2rpm.prediction.kos) = gnl
rownames(log2rpm.prediction.kos) = rownames(at)
save(log2rpm.prediction.kos, file = "Os2016/Os2016_FIT_output/kos_BG_model_prediction")

###Load data, merge and save
load("Os2015/Os2015_FIT_output/tak_BG_model")
prediction <- FIT::predict(models.rnaseq, prediction.attribute, prediction.weather)
log2rpm.prediction.tak = t(matrix(unlist(prediction),byrow = T, nrow = length(prediction)))
colnames(log2rpm.prediction.tak) = gnl
rownames(log2rpm.prediction.tak) = rownames(at)
save(log2rpm.prediction.tak, file = "Os2016/Os2016_FIT_output/tak_BG_model_prediction")

################ Make Fig.3c
# load data --------------
# Load FIT result (observed)
load("Os2016/Os2016_FIT_output/kos_BG_model_prediction")
load("Os2016/Os2016_FIT_output/tak_BG_model_prediction")
load("Os2016/Os2016_FIT_input/log2rpm_all")
log2rpm.all = log2rpm.all[,colnames(log2rpm.prediction.kos)]
load("Os2016/Os2016_FIT_input/SampleAttribute_all")
at = at[at$LineName!="Unpredictable",]


#Caluculate residial errors of all attribute
#Kos obeserved exp - Kos or Tak predicted exp
rsd.kos <- abs(log2rpm.prediction.kos[rownames(at),] -log2rpm.all[rownames(at),]) 
#Tak obeserved exp - Kos or Tak predicted exp
rsd.tak <- abs(log2rpm.prediction.tak[rownames(at),] - log2rpm.all[rownames(at),])

q.value.sig = q.value[,colSums(q.value<FDR)>0]
genes_with_Markers = colnames(q.value.sig)

res = matrix(0,ncol = length(genes_with_Markers), nrow = 2)
rownames(res) = c("bg","edQTL")
genes_with_Markers = sort(genes_with_Markers)
colnames(res) = genes_with_Markers
for(i in 1:length(genes_with_Markers)){
  gn = genes_with_Markers[i]
  gn.makers.q.vakue = q.value.sig[,gn]
  names(gn.makers.q.vakue) = seq(1,length(gn.makers.q.vakue))
  gn.makers = gn.makers.q.vakue<FDR
  linename = at$LineName
  marker =prediction.model.mat[gn,linename]
  bg = sum(rsd.kos[is.element(at$LineName, CSSLs.kos.BG),gn],rsd.tak[is.element(at$LineName, CSSLs.kos.BG),gn])
  edQTL = sum(rsd.kos[prediction.model.mat[gn,at$LineName[is.element(at$TransPlantSet,c(1,2))]]=="A",gn],rsd.tak[prediction.model.mat[gn,at$LineName[is.element(at$TransPlantSet,c(1,2))]]=="B",gn])
  res[,i] = c(bg,edQTL)
}

pdf("Figures/fig3c.pdf", height = 7, width = 14)
layout(matrix(c(rep(1,3),rep(2,3),rep(1,3),rep(3,3),rep(1,3),rep(4,3)), byrow = T, ncol =6))
dist = apply(abs(log2rpm.prediction.kos[,genes_with_Markers] - log2rpm.prediction.tak[,genes_with_Markers]),2,mean)
seq.chr = (cumsum(table(unlist(lapply(strsplit(gsub("Os", "", genes_with_Markers),"g"), FUN = function(x) {return(as.numeric(x[1]))}))))+0.5)[1:11]
plot((res[1,]-res[2,])/dist, pch = 16, sub = sprintf("%s / %s",sum(res[1,]>res[2,]),length(genes_with_Markers)), type = "n", ylim=c(-200,200))
abline(v=seq.chr, col = "gray", lty="dashed")
points((res[1,]-res[2,])/dist, pch = 16, sub = sprintf("%s / %s",sum(res[1,]>res[2,]),length(genes_with_Markers)))
abline(h = 0, col = "red", lwd = 2)
hist((res[1,]-res[2,])/dist, xlim = c(200, -200), col = "black", breaks = 50)
abline(v = 0, col = "red", lwd = 2)
dev.off()

################ Make sup fig.8
#2015
source("scripts/scaled_age_2015.R")
# load expression data table
load("Os2015/Os2015_FIT_input/log2rpm_all")
gene.id = colnames(log2rpm.all)

load("Os2015/Os2015_FIT_input/SampleAttribute_all")

# Load prediction model matrix
load("Os2015/prediction_model_mat")

load("Os2015/Os2015_FIT_output/kos_BG_model")
models.rnaseq.kos = models.rnaseq

load("Os2015/Os2015_FIT_output/tak_BG_model")
models.rnaseq.tak = models.rnaseq

#### Predict expression dynamics of each genotype
# Prep attribute of each genotype for predction
at.FIT = at
for(i in 1:nrow(at)){
  tmp = at[i,]
  lineName = tmp$LineName
  data = sprintf("%04d-%02d-%02d", tmp$year, tmp$month, tmp$day)
  age = pesudoAge(c(data, lineName, tmp$TransPlantSet))
  tmp$age = age
  at.FIT[i,] = tmp
}  
save(at.FIT, file = "Os2015/Os2015_FIT_input/attribute_for_prediction")

#FIT prediction
log2rpm.prediction = matrix(0, nrow = nrow(at), ncol = length(gene.id))
colnames(log2rpm.prediction) = gene.id
rownames(log2rpm.prediction) = rownames(at)

#Load attribute information
prediction.attribute  <- FIT::load.attribute("Os2015/Os2015_FIT_input/attribute_for_prediction", 'at.FIT')

#Load wether information
prediction.weather    <- FIT::load.weather("Os2015/Os2015_FIT_input/wtdata_2015_Takatsuki", 'wt', c('temperature', 'radiation'))

# Koshihikari model
prediction <- FIT::predict(models.rnaseq.kos, prediction.attribute, prediction.weather)
log2rpm.prediction.kos = matrix(unlist(prediction),byrow = F, nrow = nrow(at.FIT))
colnames(log2rpm.prediction.kos) = gene.id
rownames(log2rpm.prediction.kos) = rownames(at.FIT)

# Takanaei model
prediction <- FIT::predict(models.rnaseq.tak, prediction.attribute, prediction.weather)
log2rpm.prediction.tak = matrix(unlist(prediction),byrow = F, nrow = nrow(at.FIT))
colnames(log2rpm.prediction.tak) = gene.id
rownames(log2rpm.prediction.tak) = rownames(at.FIT)

# model selection
for(i in 1:nrow(at.FIT)){
  lineName = at.FIT[i,]$LineName
  log2rpm.prediction[i, prediction.model.mat[,lineName]=="A"] = log2rpm.prediction.kos[i,prediction.model.mat[,lineName]=="A"]
  log2rpm.prediction[i, prediction.model.mat[,lineName]=="B"] = log2rpm.prediction.tak[i,prediction.model.mat[,lineName]=="B"]
}

log2rpm.prediction.2015 = log2rpm.prediction
save(log2rpm.prediction.2015, file = sprintf("Os2015/Os2015_FIT_output/%s_prediction", "Os2015_observed_allgene"))

#2016 FIT
#### Predict expression dynamics of each genotype
# Prep attribute of each genotype for predction
source("scripts/scaled_age_2016.R")
load("Os2016/Os2016_FIT_input/SampleAttribute_all")
at = at[at$LineName!="Unpredictable",]
for(i in 1:nrow(at)){
  tmp = at[i,]
  lineName = tmp$LineName
  data = sprintf("%04d-%02d-%02d", tmp$year, tmp$month, tmp$day)
  age = pesudoAge(c(data, lineName, tmp$TransPlantSet))
  tmp$age = age
  at.FIT[i,] = tmp
}  
save(at.FIT, file = "Os2016/Os2016_FIT_input/attribute_for_prediction")

#FIT prediction
log2rpm.prediction = matrix(0, nrow = nrow(at), ncol = length(gene.id))
colnames(log2rpm.prediction) = gene.id
rownames(log2rpm.prediction) = rownames(at)

#####Parameters##########
#Load attribute information
prediction.attribute  <- FIT::load.attribute("Os2016/Os2016_FIT_input/attribute_for_prediction", 'at.FIT')

#Load wether information
prediction.weather    <- FIT::load.weather("Os2016/Os2016_FIT_input/wtdata_2016_Kizugawa", 'wt', c('temperature', 'radiation'))

# Koshihikari model
prediction <- FIT::predict(models.rnaseq.kos, prediction.attribute, prediction.weather)
log2rpm.prediction.kos = matrix(unlist(prediction),byrow = F, nrow = nrow(at.FIT))
colnames(log2rpm.prediction.kos) = gene.id
rownames(log2rpm.prediction.kos) = rownames(at.FIT)

# Takanaei model
prediction <- FIT::predict(models.rnaseq.tak, prediction.attribute, prediction.weather)
log2rpm.prediction.tak = matrix(unlist(prediction),byrow = F, nrow = nrow(at.FIT))
colnames(log2rpm.prediction.tak) = gene.id
rownames(log2rpm.prediction.tak) = rownames(at.FIT)

# model selection
for(i in 1:nrow(at.FIT)){
  lineName = at.FIT[i,]$LineName
  if(lineName!="Unpredictable"){
    log2rpm.prediction[i, prediction.model.mat[,lineName]=="A"] = log2rpm.prediction.kos[i,prediction.model.mat[,lineName]=="A"]
    log2rpm.prediction[i, prediction.model.mat[,lineName]=="B"] = log2rpm.prediction.tak[i,prediction.model.mat[,lineName]=="B"]
  } else {
    log2rpm.prediction[i,] = log2rpm.prediction.kos[i,]
  }
}

log2rpm.prediction.2016 = log2rpm.prediction
save(log2rpm.prediction.2016, file = sprintf("Os2016/Os2016_FIT_output/%s_prediction", "Os2016_observed_allgene"))

#Calculate prediction error in 2015
load("Os2015/Os2015_FIT_input/log2rpm_all")
load("Os2016/Os2016_FIT_input/SampleAttribute_all")
prediction.error.2015 = abs(log2rpm.all[rownames(log2rpm.prediction.2015),]-log2rpm.prediction.2015)
prediction.error.2015.aug = prediction.error.2015[at$month==8,]
prediction.error.2015.aug.geneMean = apply(prediction.error.2015,2,median)

#Calculate prediction error in 2016
load("Os2016/Os2016_FIT_input/log2rpm_all")
load("Os2016/Os2016_FIT_input/SampleAttribute_all")
log2rpm = log2rpm.all[rownames(at),]
prediction.error.2016 = abs(log2rpm-log2rpm.prediction.2016)
prediction.error.2016.aug = prediction.error.2016[is.element(at$TransPlantSet, c(1,3)),]
prediction.error.2016.aug.geneMean = apply(prediction.error.2016,2,median)

#Prep. list of gene with edQTL
load("Os2015/pqvalue")
FDR = 0.05
gnl_edQTL = colnames(q.value)[colSums(q.value<FDR)>0]

source("scripts/colname2rgb.R")
pdf("Figures/supfig8.pdf")
range = c(0,max(c(prediction.error.2015.aug.geneMean, prediction.error.2016.aug.geneMean)))
plot(prediction.error.2015.aug.geneMean, prediction.error.2016.aug.geneMean,xlim =range, ylim = range, pch=16, main = "Median of prediction error", col = colnames2rgb("gray",100))
points(prediction.error.2015.aug.geneMean[gnl_edQTL], prediction.error.2016.aug.geneMean[gnl_edQTL],  pch=16, col = "orange")
lines(c(-100,100),c(-100,100), cex = 0.5)
range = c(0,10)
plot(prediction.error.2015.aug.geneMean, prediction.error.2016.aug.geneMean,xlim =range, ylim = range, pch=16, main = "Median of prediction error", col = colnames2rgb("gray",100))
points(prediction.error.2015.aug.geneMean[gnl_edQTL], prediction.error.2016.aug.geneMean[gnl_edQTL],  pch=16, col = "orange")
lines(c(-100,100),c(-100,100), cex = 0.5)
dev.off()

################ Make Fig.4ab and Sup Fig.9b
source("scripts/scaled_age_2015.R")
library("fields")
source("scripts/colname2rgb.R")

blues = c("blue","Royalblue", "skyblue", "cyan") 
reds = c("red","red3","pink1","magenta")

ng.plot.ObsPred_HP <- function(gn, ex, prediction, prediction.col, sub, col,min,max, pch,comment){
  plot(ex[,gn], ylim=c(min,max), col=col, pch = pch,
       m=sprintf("  %s\n%s", gn,  comment),
       sub=sub)
  lines(prediction[,gn], col=prediction.col)
  par(xpd=T)
  par(xpd=F)
}

#load transcript description 
load("General_input/141103-1_GeneDescription_Osa")

# load sample attribute 
load("Os2015/Os2015_FIT_input/SampleAttribute_all")
at.FIT = at[is.element(at$LineName,c("HP-a", "HP-b")),]

# Load p,q-value
load("Os2015/pqvalue")
rownames(q.value) = seq(1,nrow(q.value))

# Load log2rpm
load("Os2015/Os2015_FIT_input/rpm_all")
log2rpm = log2rpm.all[rownames(at)[is.element(at$LineName,c("HP-a", "HP-b"))],]

# genotype --------------------
lineIDkos <- unique(c("Koshihikari",at$LineName[grep("SL12",at$LineName)]))
lineIDtak <- unique(c("Takanari",at$LineName[grep("SL13",at$LineName)]))
sample.kos <- is.element(at[,"LineName"], lineIDkos)
sample.tak <- is.element(at[,"LineName"], lineIDtak)
sample.all <- is.element(at[,"LineName"], c(lineIDtak, lineIDkos))
sample.HP.a = as.character(at[,"sampleID"][is.element(at[,"LineName"], "HP-a")])
names(sample.all) = rownames(at)
names(sample.kos) = rownames(at)
names(sample.tak) = rownames(at)
sample.HP.b = as.character(at[,"sampleID"][is.element(at[,"LineName"], "HP-b")])

# load genotype matrix
gtmat <- read.delim("General_input/161117_Genotype_KosTakCSSL_HPa12.txt", header=T, as.is=T)
gt.at <- gtmat[,1:4]
gt.mat <- gtmat[,5:ncol(gtmat)] 
gt.mat[gt.mat=="A"] = 1
gt.mat[gt.mat=="B"] = 0
gt.mat.sum = colSums(gt.mat>0)
gt.mat.sum[gt.mat.sum<50] = 0
gt.mat.sum[gt.mat.sum>50] = 1
tmp = c(1,0)
names(tmp) = c("Koshihikari", "Takanari")
gt.mat.sum = c(gt.mat.sum, tmp)

#Parse the genotypes of HP-a and HP-b
gt.mat.target <- gtmat[,c("HP.a","HP.b")]
colnames(gt.mat.target) = c("HP-a","HP-b")
rownames(gt.mat.target) = seq(1,141)
gt.mat.target[gt.mat.target=="A"] = 1
gt.mat.target[gt.mat.target=="B"] = 0
gt.mat.target[gt.mat.target=="A/B"] = "-"

#Load gene model prediction matrix
load("Os2015/prediction_model_mat")

# Predict BIL expression by FIT
load("Os2015/Os2015_FIT_output/kos_BG_model")
models.rnaseq.kos = models.rnaseq

load("Os2015/Os2015_FIT_output/tak_BG_model")
models.rnaseq.tak = models.rnaseq

#### Predict expression dynamics of each genotype
# Prep attribute of each genotype for predction
for(i in 1:nrow(at.FIT)){
  tmp = at.FIT[i,]
  lineName = tmp$LineName
  data = sprintf("%04d-%02d-%02d", tmp$year, tmp$month, tmp$day)
  age = pesudoAge(c(data, lineName, tmp$TransPlantSet))
  tmp$age = age
  at.FIT[i,] = tmp
}  
save(at.FIT, file = "Os2015/Os2015_FIT_input/attribute_for_prediction")

#FIT prediction
log2rpm.prediction = matrix(0, nrow = nrow(at.FIT), ncol = ncol(log2rpm))
colnames(log2rpm.prediction) = colnames(log2rpm)
rownames(log2rpm.prediction) = rownames(at.FIT)

#Load attribute information
prediction.attribute  <- FIT::load.attribute("Os2015/Os2015_FIT_input/attribute_for_prediction", 'at.FIT')

#Load wether information
prediction.weather    <- FIT::load.weather("Os2015/Os2015_FIT_input/wtdata_2015_Takatsuki", 'wt', c('temperature', 'radiation'))

# Koshihikari model
prediction <- FIT::predict(models.rnaseq.kos, prediction.attribute, prediction.weather)
log2rpm.prediction.kos = matrix(unlist(prediction),byrow = F, nrow = nrow(at.FIT))
colnames(log2rpm.prediction.kos) = colnames(log2rpm)
rownames(log2rpm.prediction.kos) = rownames(at.FIT)

# Takanaei model
prediction <- FIT::predict(models.rnaseq.tak, prediction.attribute, prediction.weather)
log2rpm.prediction.tak = matrix(unlist(prediction),byrow = F, nrow = nrow(at.FIT))
colnames(log2rpm.prediction.tak) = colnames(log2rpm)
rownames(log2rpm.prediction.tak) = rownames(at.FIT)
log2rpm.prediction.BIL = list(kos=log2rpm.prediction.kos, tak=log2rpm.prediction.tak)
save(log2rpm.prediction.BIL, file = sprintf("Os2015/Os2015_FIT_output/%s_prediction", "BIL"))

#Kos obeserved exp - Kos or Tak predicted exp
rsd.kos <- abs(log2rpm.prediction.kos[rownames(at.FIT),] -log2rpm[rownames(at.FIT),]) 
#Tak obeserved exp - Kos or Tak predicted exp
rsd.tak <- abs(log2rpm.prediction.tak[rownames(at.FIT),] - log2rpm[rownames(at.FIT),])

##### Make fig.4ab 
gnl = c("Os12g0406000","Os04g0611700")
pdf("Figures/fig4de_Supfig9.pdf", width = 12)
for(gn in gnl){
  rpm.min = min(log2rpm)
  rpm.max = max(log2rpm)
  q.value.sig = q.value[,colSums(q.value<FDR)>0]
  genes_with_Markers = colnames(q.value.sig)
  gn.Markers.q.vakue = q.value.sig[,gn]
  names(gn.Markers.q.vakue) = seq(1,length(gn.Markers.q.vakue))
  gn.Markers = gn.Markers.q.vakue<FDR
  
  # detect peaks 
  peak.pos = NULL
  before.negative = 1
  for(z in 1:length(gn.Markers)){
    if(gn.Markers[z]==TRUE && before.negative == 1){
      sp = z
      before.negative = 0
    } 
    if(gn.Markers[z]==FALSE && before.negative == 0){
      ep = z-1
      c(sp, ep)
      peak.pos = c(peak.pos, list(c(sp, ep)))
      before.negative = 1
    }
    if(z == length(gn.Markers) && before.negative == 0){
      ep = z
      peak.pos = c(peak.pos, list(c(sp, ep)))
    }
    
  }
  #detect most siginificant marker
  peak.pos.sig = NULL
  for(i in 1:length(peak.pos)){
    sp = 1
    ep = 141
    if(sp < ep){
      q.value.tmp = q.value[sp:ep,gn]
      q.value.min = min(q.value.tmp)
      peak.pos.sig = c(peak.pos.sig,names(q.value.tmp)[q.value.tmp==q.value.min])   
    } else {
      peak.pos.sig = c(peak.pos.sig,sp)
    }
  }
  
  #Select prediction model kos or tak
  HP.a.is.kos = sum(is.element(rownames(gt.mat.target)[gt.mat.target[,1]==1] , unlist(peak.pos.sig)))
  HP.b.is.kos = sum(is.element(rownames(gt.mat.target)[gt.mat.target[,2]==1] , unlist(peak.pos.sig)))
  
  #Predict expression level based on the selected model
  if(HP.a.is.kos>0){
    HP.a.col = "blue"
    HP.a.col.c = colnames2rgb("red",20)
    HP.a.prediction = log2rpm.prediction.kos
    HP.a.prediction.c = log2rpm.prediction.tak
  } else {
    HP.a.col = "red"
    HP.a.col.c = colnames2rgb("blue",20)
    HP.a.prediction = log2rpm.prediction.tak
    HP.a.prediction.c = log2rpm.prediction.kos
  }
  if(HP.b.is.kos>0){
    HP.b.col = "blue"
    HP.b.col.c = colnames2rgb("red",20)
    HP.b.prediction = log2rpm.prediction.kos
    HP.b.prediction.c = log2rpm.prediction.tak
  } else {
    HP.b.col = "red"
    HP.b.col.c = colnames2rgb("blue",20)
    HP.b.prediction = log2rpm.prediction.tak
    HP.b.prediction.c = log2rpm.prediction.kos
  }
  
  pch = sample.kos
  pch[sample.kos==TRUE] = 19
  pch[sample.tak==TRUE] = 19
  pch[pch==FALSE] = 0
  
  #Preparation of color for kos and tak
  color = rep(colnames2rgb("white",0), length(sample.kos))
  names(color) = names(sample.kos)
  color[rownames(at)[at$LineName=="Koshihikari"]] = blues[at$TransPlantSet[at$LineName=="Koshihikari"]]
  color[rownames(at)[at$LineName=="Takanari"]] = reds[at$TransPlantSet[at$LineName=="Takanari"]]
  color.changed = color
  layout(matrix(c(1,2,3,3), nrow=2, byrow = T)) 
  sample.kos.origin <- is.element(at[,"LineID"], c(42,83))
  sample.tak.origin <- is.element(at[,"LineID"], c(43,84))
  color.kos.tak = sample.kos.origin
  color.kos.tak[sample.kos.origin==TRUE] = "blue" #blue
  color.kos.tak[sample.tak.origin==TRUE] = "red" #red
  color.kos.tak[color.kos.tak==FALSE] = "#FFFFFF00"
  pch = 16
  #Plot HP-a genotype and correlation between markers and expression difference
  HP.a.col.kostak.marker = rep("#FF000030",nrow(gt.mat))
  HP.a.col.kostak.marker[gt.mat.target[,2]==1] = "#0000FF30"
  HP.a.col.kostak.marker[((q.value[,gn] < FDR) + (gt.mat.target[,1]==1))>1] = "blue"
  range = range(-log10(q.value[,gn]))
  plot(-log10(q.value[,gn]),ylim = range, xlab = "marker", pch = 16, main = "Markers and correlation HP-a", type = "n")
  lines(-log10(q.value[,gn]), ylim = range, col = "grey")
  points(-log10(q.value[,gn]), col=HP.a.col.kostak.marker, ylim = range, pch =16)
  abline(h = -log10(0.05))
  abline(v=c(0,cumsum(table(gt.at$chr.)))+0.5, col="#666666", lty="dotted")
  HP.a.re.mean = sprintf("HP.a:  %s,  %s",round(median(rsd.tak[sample.HP.a,gn]),digits = 2), round(median(rsd.kos[sample.HP.a,gn]),digits = 2))
  
  #Plot HP-b genotype and correlation between markers and expression difference
  HP.b.col.kostak.marker = rep("#FF000030",nrow(gt.mat))
  HP.b.col.kostak.marker[gt.mat.target[,2]==1] = "#0000FF30"
  HP.b.col.kostak.marker[((q.value[,gn] < FDR) + (gt.mat.target[,2]==1))>1] = "blue"
  range = range(-log10(q.value[,gn]))
  plot(-log10(q.value[,gn]),ylim = range, xlab = "marker", pch = 16, main = "Markers and correlation HP-b", type = "n")
  lines(-log10(q.value[,gn]), ylim = range, col = "grey")
  points(-log10(q.value[,gn]), col=HP.b.col.kostak.marker, ylim = range, pch =16)
  abline(h = -log10(0.05))
  abline(v=c(0,cumsum(table(gt.at$chr.)))+0.5, col="#666666", lty="dotted")
    HP.b.re.mean = sprintf("HP. b: %s, %s",round(median(rsd.tak[sample.HP.b,gn]),digits = 2), round(median(rsd.kos[sample.HP.b,gn]),digits = 2))
  
  ex.hp = c(log2rpm[sample.HP.a,gn],log2rpm[sample.HP.b,gn])
  length(sample.HP.a) #4
  length(sample.HP.b) #6
  border = c(5.5)
  index.HP.a = seq(1,length(sample.HP.a))
  index.HP.b = seq(length(sample.HP.a) + 1,length(sample.HP.a)+  length(sample.HP.b))
  plot(ex.hp, col = "green", pch = 16, ylim = c(rpm.min, rpm.max), sub = sprintf("Median of abs. R.E. tak, kos , %s, %s",  HP.a.re.mean, HP.b.re.mean), main = gn)
  abline(v = border, col = "grey", lty = "dashed")  
  lines(index.HP.a,HP.a.prediction[sample.HP.a,gn],  col = HP.a.col)
  lines(index.HP.a,HP.a.prediction.c[sample.HP.a,gn],  col = HP.a.col.c)
  lines(index.HP.b,HP.b.prediction[sample.HP.b,gn],  col = HP.b.col)
  lines(index.HP.b,HP.b.prediction.c[sample.HP.b,gn],  col = HP.b.col.c)
}
dev.off()


#################### Make Fig.4 and SupFig10
# load data --------------
# load sample attribute 
load("Os2015/Os2015_FIT_input/SampleAttribute_all") #Include HP-a, HP-b
at.all = at

# load expression data table
load("Os2015/Os2015_FIT_input/log2rpm_all")
log2rpm = log2rpm.all[is.element(at$LineName,c("HP-a","HP-b")),]

#load transcript description 
load("General_input/141103-1_GeneDescription_Osa")

# genotype --------------------
lineIDkos <- unique(c("Koshihikari",at$LineName[grep("SL12",at$LineName)]))
lineIDtak <- unique(c("Takanari",at$LineName[grep("SL13",at$LineName)]))
sample.kos <- is.element(at[,"LineName"], lineIDkos)
sample.tak <- is.element(at[,"LineName"], lineIDtak)
sample.all <- is.element(at[,"LineName"], c(lineIDtak, lineIDkos))
sample.HP.a = as.character(at[,"sampleID"][is.element(at[,"LineName"], "HP-a")])
names(sample.all) = rownames(at)
names(sample.kos) = rownames(at)
names(sample.tak) = rownames(at)
sample.HP.a = is.element(at[,"sampleID"], as.numeric(sample.HP.a))
sample.HP.b = is.element(at[,"LineName"], "HP-b")
names(sample.HP.a) = rownames(at)
names(sample.HP.b) = rownames(at)
sample.HP.a[sample.HP.a==TRUE] = "green" 
sample.HP.b[sample.HP.b==TRUE] = "green" 
sample.HP.a[sample.HP.a==FALSE] = "#FFFFFF00"
sample.HP.b[sample.HP.b==FALSE] = "#FFFFFF00"

# load genotype matrix
gtmat <- read.delim("General_input/161117_Genotype_KosTakCSSL_HPa12.txt", header=T, as.is=T)
gt.at <- gtmat[,1:4]
gt.mat <- gtmat[,5:ncol(gtmat)] 
gt.mat[gt.mat=="A"] = 1
gt.mat[gt.mat=="B"] = 0
gt.mat.sum = colSums(gt.mat>0)
gt.mat.sum[gt.mat.sum<50] = 0
gt.mat.sum[gt.mat.sum>50] = 1
tmp = c(1,0)
names(tmp) = c("Koshihikari", "Takanari")
gt.mat.sum = c(gt.mat.sum, tmp)

#HP-a and HP-b
gt.mat.target <- gtmat[,c("HP.a","HP.b")]
colnames(gt.mat.target) = c("HP-a", "HP-b")
rownames(gt.mat.target) = seq(1,141)
gt.mat.target[gt.mat.target=="A"] = 1
gt.mat.target[gt.mat.target=="B"] = 0
gt.mat.target[gt.mat.target=="A/B"] = "-"

# Load FIT prediction matrix
load("Os2015/Os2015_FIT_output/BIL_prediction")
log2rpm.prediction.kos = log2rpm.prediction.BIL$kos
log2rpm.prediction.tak = log2rpm.prediction.BIL$tak

# Load marker info. acounting for kos/tak differences
load("Os2015/pqvalue")
FDR = 0.05
q.value.sig = q.value[,colSums(q.value<FDR)>0]
genes.with.edQTL = colnames(q.value.sig)

#Caluculate residial errors of all attribute
#Kos obeserved exp - Kos or Tak predicted exp
rsd.kos <- abs(log2rpm.prediction.kos[,genes.with.edQTL] -log2rpm.all[rownames(log2rpm.prediction.kos),genes.with.edQTL]) 
#Tak obeserved exp - Kos or Tak predicted exp
rsd.tak <- abs(log2rpm.prediction.tak[,genes.with.edQTL] -log2rpm.all[rownames(log2rpm.prediction.kos),genes.with.edQTL])

niter <- 10000 #Num. of permutation test of changing HP lines' genotypes
prediction.result = matrix(0, nrow = 2,ncol =  length(genes.with.edQTL))
rownames(prediction.result) = c("HP-a","HP-b")
colnames(prediction.result) = genes.with.edQTL
improvement = prediction.result
sim.result = matrix(0, nrow = 2, ncol = niter)

#Check length of recombinated genome of HP-a-2
tmp = NULL
first = 1
for(l in 1:nrow(gt.mat.target)){
  if(l == nrow(gt.mat.target)){
    tmp = c(tmp,1)
  } else if((gt.mat.target[l,1] == 1) && (first == 1)){
    first = 0
    sp = l
  } else if((gt.mat.target[l,1] == 1) && (first == 0)) {
  } else if((gt.mat.target[l,1] != 1) && (first == 0)) {
    ep = l - 1
    tmp = c(tmp, ep-sp +1)
    first = 1
  } else if((l == nrow(gt.mat.target)) && (first == 0)){
    ep = l
    tmp = c(tmp, ep-sp +1)
  }
}
HP.a.sim.gm = c(tmp, rep(0, nrow(gt.mat.target)-sum(tmp)))

#Check length of recombinated genome of HP-b-1
tmp = NULL
first = 1
for(l in 1:nrow(gt.mat.target)){
  if(l == nrow(gt.mat.target)){
    tmp = c(tmp,2)
  } else if((gt.mat.target[l,2] == 1) && (first == 1)){
    first = 0
    sp = l
  } else if((gt.mat.target[l,2] == 1) && (first == 0)) {
  } else if((gt.mat.target[l,2] != 1) && (first == 0)) {
    ep = l - 1
    tmp = c(tmp, ep-sp +1)
    first = 1
  } else if((l == nrow(gt.mat.target)) && (first == 0)){
    ep = l
    tmp = c(tmp, ep-sp +1)
  }
}
HP.b.sim.gm = c(tmp, rep(0, nrow(gt.mat.target)-sum(tmp)))

########Produce simulated genotype with same recobinated genome length of each line and Calulate sum of R.E.
set.seed(1234)
for(n in 1:niter){
  #HP-a
  tmp = sample(HP.a.sim.gm)
  tmp2 = NULL
  for(a in 1:length(tmp)){
    if(tmp[a]>0){
      tmp2 = c(tmp2, rep(1,tmp[a]))
    } else {
      tmp2 = c(tmp2, 0)
    }
  }
  gt.mat.target.sim = gt.mat.target
  gt.mat.target.sim[,1] = tmp2 
  
  #HP-b
  tmp = sample(HP.a.sim.gm)
  tmp2 = NULL
  for(a in 1:length(tmp)){
    if(tmp[a]>0){
      tmp2 = c(tmp2, rep(1,tmp[a]))
    } else {
      tmp2 = c(tmp2, 0)
    }
  }
  gt.mat.target.sim[,2] = tmp2 
  
  ############ Caluculate residual error of each gene with simulated genotype 
  for(g in 1:length(genes.with.edQTL)){
    gn = genes.with.edQTL[g]
    gn.markers.q.vakue = q.value.sig[,gn]
    names(gn.markers.q.vakue) = seq(1,length(gn.markers.q.vakue))
    gn.markers = gn.markers.q.vakue<FDR
    
    # detect peaks 
    peak.pos = NULL
    before.negative = 1
    for(z in 1:length(gn.markers)){
      if(gn.markers[z]==TRUE && before.negative == 1){
        sp = z
        before.negative = 0
      } 
      if(gn.markers[z]==FALSE && before.negative == 0){
        ep = z-1
        c(sp, ep)
        peak.pos = c(peak.pos, list(c(sp, ep)))
        before.negative = 1
      }
      if(z == length(gn.markers) && before.negative == 0){
        ep = z
        peak.pos = c(peak.pos, list(c(sp, ep)))
      }
    }
    #detect most siginificant marker in each peak
    peak.pos.sig = NULL
    for(i in 1:length(peak.pos)){
      sp = 1
      ep = 141
      if(sp < ep){
        q.value.tmp = q.value[sp:ep,gn]
        q.value.min = min(q.value.tmp)
        peak.pos.sig = c(peak.pos.sig,(sp:ep)[q.value.tmp==q.value.min])   
      } else {
        peak.pos.sig = c(peak.pos.sig,sp)
      }
    }
    
    #Select prediction model of HP-A-2
    HP.a.is.kos = 0
    tmp = NULL
    for(f in 1:length(unlist(peak.pos.sig))){
      i = 1
      if(f > 1){
        if(unlist(peak.pos.sig)[f]-1 == unlist(peak.pos.sig)[f-1]){
          tmp = c(tmp, f)
        } else {
          if(sum(is.element(rownames(gt.mat.target.sim)[gt.mat.target.sim[,1]==1],unlist(peak.pos.sig)[tmp]))== length(tmp)){
            HP.a.is.kos = HP.a.is.kos + 1
          }
          tmp = c(f)
        }
      } else {
        tmp = c(tmp, f)
      }
    }
    if(sum(is.element(rownames(gt.mat.target.sim)[gt.mat.target.sim[,1]==1],unlist(peak.pos.sig)[tmp])) == length(tmp)){
      HP.a.is.kos = HP.a.is.kos + 1
    }
    
    #Select prediction model of HP-b-1
    HP.b.is.kos = 0
    tmp = NULL
    for(f in 1:length(unlist(peak.pos.sig))){
      i = 1
      if(f > 1){
        if(unlist(peak.pos.sig)[f]-1 == unlist(peak.pos.sig)[f-1]){
          tmp = c(tmp, f)
        } else {
          if(sum(is.element(rownames(gt.mat.target.sim)[gt.mat.target.sim[,2]==1],unlist(peak.pos.sig)[tmp]))== length(tmp)){
            HP.b.is.kos = HP.b.is.kos + 1
          }
          tmp = c(f)
        }
      } else {
        tmp = c(tmp, f)
      }
    }
    if(sum(is.element(rownames(gt.mat.target.sim)[gt.mat.target.sim[,2]==1],unlist(peak.pos.sig)[tmp])) == length(tmp)){
      HP.b.is.kos = HP.b.is.kos + 1
    }
    
    #Calulate median of R.E. for each gene og each simulated genotye
    rsd.kos.HP.a  = median(rsd.kos[names(sample.HP.a)[sample.HP.a=="green"],gn])
    rsd.tak.HP.a  = median(rsd.tak[names(sample.HP.a)[sample.HP.a=="green"],gn])
    rsd.kos.HP.b  = median(rsd.kos[names(sample.HP.b)[sample.HP.b=="green"],gn])
    rsd.tak.HP.b  = median(rsd.tak[names(sample.HP.b)[sample.HP.b=="green"],gn])
    prediction.result[,gn] = c(HP.a.is.kos,HP.b.is.kos)
    
    #Save median of simulated R.E. 
    if(HP.a.is.kos >0){
      improvement[1,gn] = rsd.kos.HP.a
    } else {
      improvement[1,gn] = rsd.tak.HP.a
    }
    if(HP.b.is.kos >0){
      improvement[2,gn] = rsd.kos.HP.b
    } else {
      improvement[2,gn] = rsd.tak.HP.b
    }
  }
  HP.a.sim = sum(improvement[1,])
  HP.b.sim = sum(improvement[2,])
  
  sim.result[,n] = c(HP.a.sim, HP.b.sim)
  cat(sprintf("%s : %s\n",n , niter))   
}


###### #Secelct prediction model based on each HP genotype
# Load model selection matrix
load("Os2015/prediction_model_mat")
prediction.model.mat.edQTL = prediction.model.mat[genes.with.edQTL,]

#Save median of simulated R.E. 
HP.a = sum(c(apply(rsd.tak[rownames(at)[at$LineName=="HP-a"],rownames(prediction.model.mat.edQTL)[prediction.model.mat.edQTL[,"HP-a"]=="B"]],2,median),apply(rsd.kos[rownames(at)[at$LineName=="HP-a"],rownames(prediction.model.mat.edQTL)[prediction.model.mat.edQTL[,"HP-a"]=="A"]],2,median)))
HP.b = sum(c(apply(rsd.tak[rownames(at)[at$LineName=="HP-b"],rownames(prediction.model.mat.edQTL)[prediction.model.mat.edQTL[,"HP-b"]=="B"]],2,median),apply(rsd.kos[rownames(at)[at$LineName=="HP-b"],rownames(prediction.model.mat.edQTL)[prediction.model.mat.edQTL[,"HP-b"]=="A"]],2,median)))

#Sum of prediction errors based on background (tak) genotype
HP.a.bg = sum(apply(rsd.tak[rownames(at)[at$LineName=="HP-a"],genes.with.edQTL],2,median))
HP.b.bg = sum(apply(rsd.tak[rownames(at)[at$LineName=="HP-b"],genes.with.edQTL],2,median))

#Sum of prediction errors based on background (tak) genotype
HP.a.kos = sum(apply(rsd.kos[rownames(at)[at$LineName=="HP-a"],genes.with.edQTL],2,median))
HP.b.kos = sum(apply(rsd.kos[rownames(at)[at$LineName=="HP-b"],genes.with.edQTL],2,median))

pdf(sprintf("Figures/fig4c.pdf",FDR))
hist(sim.result[1,],main = "Kos-Tak model prediction based on permutation test in HP-a", sub = "Dashed line indicate 0.001 in permuation test",xlim = c(1500,6500), xlab = "Sum of R.E.")
abline(v = quantile(sim.result[1,],probs = 0.001), col = "black", lty = "dotted", lwd = 3)
abline(v = HP.a.kos, col = "blue", lwd = 3)
abline(v = HP.a.bg, col = "red", lwd = 3)
abline(v = HP.a, col = "orange", lwd = 3)
dev.off()

pdf(sprintf("Figures/Sup_fig10.pdf",FDR))
hist(sim.result[2,],main = "Kos-Tak model prediction based on permutation test in HP-b", sub = "Dashed line indicate 0.001 in permuation test", xlim = c(1500,6500), xlab = "Sum of R.E.")
abline(v = quantile(sim.result[2,],probs = 0.001), col = "black", lty = "dotted", lwd = 3)
abline(v = HP.b.kos, col = "blue", lwd = 3)
abline(v = HP.b.bg, col = "red", lwd = 3)
abline(v = HP.b, col = "orange", lwd = 3)
dev.off()



