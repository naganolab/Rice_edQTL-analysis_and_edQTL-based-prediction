library("vcfR")
load("kostak_specific_snp")
# load("20171212_vcf")
load("20171212_gt")
gt = gt[is.element(rownames(gt),names(gt.kos.specific)),]
gt.hetero = gt
gt.homo = gt
gt.hetero[!is.element(gt, c("A/A","G/G","C/C","T/T","."))] = 1
gt.hetero[is.element(gt, c("A/A","G/G","C/C","T/T","."))] = 0
gt.homo[is.element(gt, c("A/A","G/G","C/C","T/T"))] = 1
gt.homo[!is.element(gt, c("A/A","G/G","C/C","T/T"))] = 0

#Load index info
index.info = read.table("input_data/170130_Sample-IndexPrimer.txt", header = T, stringsAsFactors = F)
index.info.tmp = matrix(0,ncol = 1,nrow = nrow(index.info))
index.info.tmp[,1] = index.info[,1]
rownames(index.info.tmp) = sprintf("%s_%03d", index.info[,2], index.info[,3])
index.info = index.info.tmp[,1]
rm(index.info.tmp)

#Load genome length info
genome.length = read.table("input_data/IRGSP-1.0_genome_Pl_Mt.fasta.fai", stringsAsFactors = F, header = F)
tmp = genome.length[,1]
genome.length = genome.length[,2]
names(genome.length) = tmp
genome.length.sum = genome.length[1:12]
for(i in 1:11){
  genome.length.sum[i+1] = sum(genome.length[1:i])
}
genome.length.sum[1] = 0

#Marge snp data from replicates
load("lineName_prediction")
lineName.prediction = lineName.prediction[,!is.element(lineName.prediction[1,], c( "Unpredictable"))]
lineID = unique(lineName.prediction[1,])
all.sample = unique(index.info) 
gt.homo.count = matrix(0, ncol = length(lineID),nrow = nrow(gt))
rownames(gt.homo.count) = rownames(gt)
colnames(gt.homo.count) = lineID
gt.hetero.count = gt.homo.count
for(i in 1:length(lineID)){
  line = lineID[i]
  sampleID = colnames(lineName.prediction)[lineName.prediction[1,]==line]
  tmp1 = apply(gt.homo[,names(index.info)[is.element(index.info, sampleID)]],1,FUN = function(x){return(sum(as.numeric(x)))})
  tmp2 = apply(gt.hetero[,names(index.info)[is.element(index.info, sampleID)]],1,FUN = function(x){return(sum(as.numeric(x)))})
  gt.homo.count[,i] = tmp1
  gt.hetero.count[,i] = tmp2
  cat(sprintf("%s / %s\n\r", i, length(lineID)))
}
rm(gt.tmp)
gc()
gc()
total.count = gt.homo.count + gt.hetero.count
hetero.ratio = gt.hetero.count/(gt.homo.count + gt.hetero.count)

#Prepare kos- and tak- specific SNP table on each chr.
chr.pos = matrix(unlist(strsplit(rownames(total.count), "_")), ncol = 2, byrow = T)
chr.pos[,1] = as.numeric(gsub("chr","",chr.pos[,1]))
colnames(chr.pos) = c("chr","pos")
rownames(chr.pos) = rownames(total.count)
total.count = total.count[!is.na(chr.pos[,1]),]
hetero.ratio = hetero.ratio[!is.na(chr.pos[,1]),]
chr.pos = chr.pos[!is.na(chr.pos[,1]),]
plotx = rep(0, nrow(chr.pos))
names(plotx) = rownames(chr.pos)
for(i in 1:nrow(chr.pos)){
  plotx[i] = as.numeric(chr.pos[i,2]) + genome.length.sum[as.numeric(chr.pos[i,1])]
}

#Caluculate hetero freq. with siliding window method
tmp = hetero.ratio
tmp[total.count<5] = NA
tmp = tmp[names(gt.kos.specific)[is.element(names(gt.kos.specific), rownames(tmp))],]
plotx = plotx[is.element(names(plotx), rownames(tmp))]
window.size = 1000000
step = 100000
sw.pos = seq(step+1,sum(genome.length), by = step)
sw = matrix(0, ncol =  ncol(hetero.ratio), nrow = length(sw.pos))
colnames(sw) = colnames(hetero.ratio)
mean.hetero = function(x){
  if(sum(!is.na(x))>2){
    return(mean(as.numeric(x[!is.na(x)])))
} else {
  return(NA)
  }
}

for(n in 1:length(sw.pos)){
  min = sw.pos[n]-window.size/2
  max = sw.pos[n]+window.size/2
  pos = names(plotx)[plotx>min & plotx<max]
  if(length(pos)<=2){
    sw[n,] = rep(NA, ncol(sw))
  } else {
    sw[n,] = apply(tmp[pos,],2, FUN = mean.hetero)
  }
}
pdf("hetero-ratio.pdf")
for(i in sort(lineID)){
  x = sw.pos[!is.na(sw[,i])]
  y = sw[!is.na(sw[,i]),i]
  if(length(x)>0){
    plot(x, y, pch = 16, type = "n", main = i, ylim = c(0,1),ylab = "hetero SNPs/all kos/tak distinguishable SNPs in 1M wondow")
    lines(x, y)
    abline(v = genome.length.sum, lty = "dashed", col = "gray")
  }
}
dev.off()

