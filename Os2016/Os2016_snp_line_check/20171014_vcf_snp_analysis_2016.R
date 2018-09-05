###########Summary of the analysis
# 1.Marge vcfs of each sample and pick up homo SNPs.
# 2.Extract SNPs.
# 3.Substract 8361 kos-specific and tak-specific SNPs at same locus detected more than 5 samples of kos or tak.
# 4.Checking the above SNPs from -0.5M to +0.5M of each marker to distigush the origins.
# 5.Predict linenames based on marker compositions.
###########


#Load vcf
library(vcfR)
vcf <- read.vcfR("Os2016_minDP3.recode.vcf.gz", verbose = FALSE, limit = 100000000000000 )
save(vcf, file = "20171228_vcf")
#Extract genotype data
gt <- extract.gt(vcf, return.alleles = TRUE)
save(gt, file = "20171208_gt")

sum(!is.element(gt,c(".")))
#Load index info
index.info = read.table("input_data/171208_Sample-IndexPrimer.txt", header = T, stringsAsFactors = F)
index.info.tmp = index.info[,1]
# index.info.tmp[,1] = index.info[,1]
names(index.info.tmp) = index.info$prefix
# index.info.tmp[,2] = sprintf("%s_%03d", index.info[,3], index.info[,4])
index.info = index.info.tmp
rm(index.info.tmp)

#Marge snp data from replicates
all.sample = unique(index.info)
marge = function(x){
  tmp = unique(x[!is.element(x,c("."))])
  if(length(tmp)==1){ #Only consistent snps are accepted. 
    return(tmp)
  } else {
    return(".")
  }
}
gt.tmp = matrix(".", ncol = length(all.sample),nrow = nrow(gt))
for(i in 1:length(all.sample)){
  tmp = gt[,names(index.info)[is.element(index.info, all.sample[i])]]
  if(class(tmp)=="matrix"){
    tmp = apply(tmp,1,FUN = marge)
  } 
  gt.tmp[,i] = tmp
}
colnames(gt.tmp) = all.sample
rownames(gt.tmp) = rownames(gt)
gt = gt.tmp
save(gt, file = "20171228_gt_marged")
rm(gt.tmp)
gc()
gc()

#Load attribute data
at = read.table("input_data/160427_sample_attribute_2016_Kizu.txt", stringsAsFactors = F, header = T, fill = T)

#Load genome length info
genome.length = read.table("input_data/IRGSP-1.0_genome_Pl_Mt.fasta.fai", stringsAsFactors = F, header = F)
tmp = genome.length[,1]
genome.length = genome.length[,2]
names(genome.length) = tmp

# #kos-specific and tak-specific SNP call
load("kostak_specific_snp")

#Prepare kos- and tak- specific SNP table on each chr.
chr.pos = matrix(unlist(strsplit(names(gt.kos.specific), "_")), ncol = 2, byrow = T)
colnames(chr.pos) = c("chr","pos")
chr.name = unique(chr.pos[,1])

#Extract kos- and tak- specific SNP from all genotype data
gt.kos.specific = gt.kos.specific[is.element(names(gt.kos.specific), rownames(gt))]
gt.tak.specific = gt.tak.specific[is.element(names(gt.tak.specific), rownames(gt))]
gt = gt[names(gt.kos.specific)[is.element(names(gt.kos.specific), rownames(gt))],] #232,313 SNPs
Line.identifier = function(x){
  x[x==gt.kos.specific] = "blue"
  x[x==gt.tak.specific] = "red"
  x[!is.element(x,c("blue","red"))] = rgb(1, 0, 0, alpha=0)
  return(x)
}
gt.identified = apply(gt,2, FUN = Line.identifier)
save(gt.identified, file = "gt_identified")

#Load CSSL map
cssl.gt.info = read.table("input_data/161117_Genotype_KosTakCSSL.txt",stringsAsFactors = F, header = T)
chr.pos = matrix(as.integer(gsub("chr","", unlist(strsplit(rownames(gt.identified),"_")))),ncol = 2, byrow = T) #gt pos
rownames(chr.pos) = rownames(gt.identified)
chr.pos = chr.pos[chr.pos[,1]!="mt",]
marker.composition = matrix("", nrow = nrow(cssl.gt.info), ncol = ncol(gt.identified))
colnames(marker.composition) = as.integer(colnames(gt.identified))

judge_kostak = function(x){
  total.count = 7
  n = 0
  gt.kostak = x[is.element(x, c("blue","red"))]
  count = c(0,0) #blue, red
  names(count) = c("A", "B")
  while(max(count)< total.count & n < length(gt.kostak)){
    n = n + 1
    if(gt.kostak[n] == "blue"){
      count[1] = count[1] + 1
    } else {
      count[2] = count[2] + 1
    }
  }
  if((count[1] == count[2]) | max(count)<total.count){
    return("A/B")
  } else {
    return(names(count)[count == max(count)])
  }
}
# judge_kostak = function(x){
#   gt.kostak = x[is.element(x, c("blue","red"))] 
#   count = table(gt.kostak)
#   if(length(count)==0){
#     count = c(0,0)
#   }
#   if((length(count)>1)&(count[1]==count[2])){
#     return("A/B")
#   } else if(max(count)<5){
#     return("A/B")
#   } else {
#     max = count[count ==max(count)]
#     if(names(max)=="blue"){
#       return("A")
#     }  else {
#       return("B")
#     }
#   }
# }
#Prep marker map
max = 1.5*10^6
for(i in 1:nrow(cssl.gt.info)){
  chr = cssl.gt.info[i,2]
  pos = cssl.gt.info[i,3]*1000000
  chr.pos.tmp = chr.pos[as.integer(chr.pos[,1])==chr,]
  chr.pos.tmp = cbind(chr.pos.tmp, abs(as.numeric(chr.pos.tmp[,2])-pos))  
  chr.pos.tmp = chr.pos.tmp[as.integer(chr.pos.tmp[,3])<max,]
  if(class(chr.pos.tmp)!="character"){
    chr.pos.tmp = chr.pos.tmp[order(as.numeric(chr.pos.tmp[,3])),]
  }
  gt.snp.near.marker = gt.identified[rownames(chr.pos.tmp)[is.element(rownames(chr.pos.tmp),rownames(gt.identified))],]
  tmp = apply(gt.snp.near.marker,2,FUN = judge_kostak)
  marker.composition[i,] = tmp
}
save(marker.composition, file = "marker_composition")

#Imputation of marker.composition
marker.list = list()
for(i in 1:12){
  tmp = list(rownames(cssl.gt.info)[cssl.gt.info[,2]==i])
  names(tmp) = i
  marker.list = c(marker.list,tmp)
}
imp = function(x){
  #1201
  if(sum(is.element(x[2:3],c("B")))>1 & x[4]=="A"){
    x[1] = "B"
  }
  
  #1229
  if(sum(is.element(x[95:96],c("B")))==2 & sum(!is.element(x[97:98],c("B")))>0){
    x[92:94] = "B"
  }
  #1321
  if(sum(is.element(x[15:16],c("A")))>0 & sum(is.element(x[72:77],c("A")))>=4 & (names(table(x))[table(x)==max(table(x))]=="B")){
    x[15:16] = "A"
    x[72:73] = "A"
  }
  #ALL
  tmp = x[x!="A/B"]
  if(length(tmp)>0){
    count = table(tmp)
    bg = names(count)[count==max(count)]
    if(length(bg)==1){
      if(bg == "A"){
        sl = "B"
      } else {
        sl = "A"
      }
      for(j in 1:12){
        for(n in marker.list[[j]][1]:marker.list[[j]][length(marker.list[[j]])-2]){
          if((x[n] == sl) && (x[n+2] == sl)){
            x[n+1] = sl
          }
          if((x[n] == bg) && (x[n+2] == bg)){
            x[n+1] = bg
          }
        }  
      }
    }
  }
  
  return(x)
}
marker.composition = apply(marker.composition,2,FUN = imp)

#Determination of LineName
lineName.prediction = marker.composition[c(1,2),]
for(i in 1:ncol(marker.composition)){
  pos.pass = (marker.composition[,i] != "A/B")
  if(sum(pos.pass)>0){
    query = marker.composition[pos.pass,i]
    cssl.check = colSums(query == cssl.gt.info[pos.pass,])
    if(sum(cssl.check == max(cssl.check))==1){
      lineName.prediction[1,i] =  colnames(cssl.gt.info)[cssl.check == max(cssl.check)]
      lineName.prediction[2,i] =  sprintf("%s/%s",max(cssl.check),sum(pos.pass))
    } else {
      lineName.prediction[1,i] =  "Unpredictable"
      lineName.prediction[2,i] =  sprintf("0/%s",sum(pos.pass))
    }
  } else{
    lineName.prediction[1,i] =  "Unpredictable"
    lineName.prediction[2,i] =  sprintf("0/%s",141)
  }
  
}
prediction.score = matrix(as.integer(unlist(strsplit(lineName.prediction[2,],"/"))),byrow = T,ncol = 2)
save(lineName.prediction, prediction.score,file = "lineName_prediction")

#Plot SNP map of each sample
chr.pos = matrix(unlist(strsplit(rownames(gt.identified), "_")), ncol = 2, byrow = T)
colnames(chr.pos) = c("chr","pos")
chr.name = unique(chr.pos[,1])


#Record VS prediction
record.lineName = at$LineName
names(record.lineName) = at$sampleID
sum(record.lineName[colnames(lineName.prediction)]==lineName.prediction[1,]) #674 samples are concictent with record
sum(lineName.prediction[1,]=="Unpredictable") #72 samples are "Unpredictable". no snp or same identity with more than one sample.

hist(prediction.score[,1]/prediction.score[,2])
sum(prediction.score[,1]/prediction.score[,2]>0.9) #879 samples are predicted with more than 90% identities.
lineName.prediction[1,(prediction.score[,1]/prediction.score[,2]<0.9)] = "Unpredictable" 
sample.kos = colnames(cssl.gt.info)[4:45]
sample.tak = colnames(cssl.gt.info)[46:85]
recored.VS.prediction = matrix("", nrow = 2, ncol = ncol(gt.identified))
rownames(recored.VS.prediction) = c("record","prediction")
colnames(recored.VS.prediction) = colnames(gt.identified)
recored.VS.prediction[1,] = record.lineName[colnames(gt.identified)]
recored.VS.prediction[2,] = lineName.prediction[1,colnames(gt.identified)]
recored.VS.prediction.background = recored.VS.prediction

#Prep. strange sample list
strange = matrix("",ncol = 2, nrow = ncol(lineName.prediction))
rownames(strange) = colnames(lineName.prediction)
colnames(strange) = c("record","prediction")
tmp = is.element(names(record.lineName), rownames(strange))
strange[names(record.lineName)[tmp],1] = record.lineName[tmp]
strange[,2] = lineName.prediction[1,]
strange = strange[strange[,1]!=strange[,2],]
write.csv(strange, file = "strange.csv")


#Plot CSSL snp map by order of record and prediction
order.record = names(sort(recored.VS.prediction[1,]))
order.prediction = names(sort(recored.VS.prediction[2,]))
order.record.strange = order.record[is.element(order.record,rownames(strange))]
order.prediction.strange = order.prediction[is.element(order.prediction,rownames(strange))]

pdf("20171020_CSSL_snp_map_by_record_strange.pdf")
for(y in 1:nrow(strange)){
  id = rownames(strange)[y]
  lineID = at[at$sampleID==id,"LineName"]
  plot(c(1,2), xlim = c(1,max(genome.length)),ylim = c(0,12), xaxt = "n", xlab = "pos. on Chr. (Mb)", ylab = "Chr. Num.",type = "n", main = sprintf("%s_%s\n%s_%s",lineID,id,lineName.prediction[1,id],lineName.prediction[2,id]))
  axis(1, at=seq(1,max(genome.length), by = 10^7), labels=c(0,10,20,30,40), las = TRUE)
  axis(2, at=1:length(genome.length), labels=FALSE, las = TRUE)
  for(i in 1:length(chr.name)){
    pos = sort(as.numeric(chr.pos[chr.pos[,1]==chr.name[i],2]))
    points(pos, rep(i,length(pos)),  pch ="|", col = gt.identified[sprintf("%s_%s",chr.name[i], pos),id]) 
    par(new = T)
    points(c(1,genome.length[i]),c(i,i),pch = "|", cex = 2, col= "gray")
    points(c(1,genome.length[i]),c(i,i),pch = "|", cex = 2, col= "gray")
  }
  col.marker = marker.composition[,id]
  col.marker[col.marker=="A"] = "blue"
  col.marker[col.marker=="A/B"] = "green"
  col.marker[col.marker=="B"] = "red"
  points(cssl.gt.info[,3]*10^6, cssl.gt.info[,2], pch = "|", cex = 2, col= col.marker)
  lines(c(0,max),c(0,0), col = "black")
  points(c(0,max),c(0,0), pch = "|" , col = "black")
}
dev.off()

pdf("20171020_CSSL_snp_map_by_record.pdf")
for(y in 1:length(order.record)){
  id = order.record[y]
  lineID = at[at$sampleID==id,"LineName"]
  plot(c(1,2), xlim = c(1,max(genome.length)),ylim = c(0,12), xaxt = "n", xlab = "pos. on Chr. (Mb)", ylab = "Chr. Num.",type = "n", main = sprintf("%s_%s\n%s_%s",lineID,id,lineName.prediction[1,id],lineName.prediction[2,id]))
  axis(1, at=seq(1,max(genome.length), by = 10^7), labels=c(0,10,20,30,40), las = TRUE)
  axis(2, at=1:length(genome.length), labels=FALSE, las = TRUE)
  for(i in 1:length(chr.name)){
    pos = sort(as.numeric(chr.pos[chr.pos[,1]==chr.name[i],2]))
    points(pos, rep(i,length(pos)),  pch ="|", col = gt.identified[sprintf("%s_%s",chr.name[i], pos),id]) 
    par(new = T)
    points(c(1,genome.length[i]),c(i,i),pch = "|", cex = 2, col= "gray")
    points(c(1,genome.length[i]),c(i,i),pch = "|", cex = 2, col= "gray")
  }
  col.marker = marker.composition[,id]
  col.marker[col.marker=="A"] = "blue"
  col.marker[col.marker=="A/B"] = "green"
  col.marker[col.marker=="B"] = "red"
  points(cssl.gt.info[,3]*10^6, cssl.gt.info[,2], pch = "|", cex = 2, col= col.marker)
  lines(c(0,max),c(0,0), col = "black")
  points(c(0,max),c(0,0), pch = "|" , col = "black")
}
dev.off()


pdf("20171020_CSSL_snp_map_by_prediction.pdf")
for(y in 1:length(order.prediction)){
  id = order.prediction[y]
  lineID = at[at$sampleID==id,"LineName"]
  plot(c(1,2), xlim = c(1,max(genome.length)),ylim = c(0,12), xaxt = "n", xlab = "pos. on Chr. (Mb)", ylab = "Chr. Num.",type = "n", main = sprintf("%s_%s\n%s_%s",lineID,id,lineName.prediction[1,id],lineName.prediction[2,id]))
  axis(1, at=seq(1,max(genome.length), by = 10^7), labels=c(0,10,20,30,40), las = TRUE)
  axis(2, at=1:length(genome.length), labels=FALSE, las = TRUE)
  for(i in 1:length(chr.name)){
    pos = sort(as.numeric(chr.pos[chr.pos[,1]==chr.name[i],2]))
    points(pos, rep(i,length(pos)),  pch ="|", col = gt.identified[sprintf("%s_%s",chr.name[i], pos),id])
    par(new = T)
    points(c(1,genome.length[i]),c(i,i),pch = "|", cex = 2, col= "gray")
    points(c(1,genome.length[i]),c(i,i),pch = "|", cex = 2, col= "gray")
  }
  col.marker = marker.composition[,id]
  col.marker[col.marker=="A"] = "blue"
  col.marker[col.marker=="A/B"] = "green"
  col.marker[col.marker=="B"] = "red"
  points(cssl.gt.info[,3]*10^6, cssl.gt.info[,2], pch = "|", cex = 2, col= col.marker)
  lines(c(0,max),c(0,0), col = "black")
  points(c(0,max),c(0,0), pch = "|" , col = "black")
}
dev.off()

