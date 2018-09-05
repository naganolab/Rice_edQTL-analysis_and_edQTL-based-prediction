#Load data
load("marker_composition")
load("lineName_prediction")
#Load CSSL map
cssl.gt.info = read.table("input_data/161117_Genotype_KosTakCSSL.txt",stringsAsFactors = F, header = T)

#Extract reliable HP.a
prediction.score = matrix(unlist(strsplit(lineName.prediction[2,],"/")), byrow = F, nrow = 2)
sample.HP.a = c("20091","20097","20743","21048","21054")
sample.HP.b = c("20025","20389","20815","21029","20701","20893")

#Check HP.a genotype
gt.check = function(x){
  tmp = x[is.element(x, c("A","B"))]
  if(length(tmp)>0){
    tmp = table(tmp)
    tmp = names(tmp)[tmp==max(tmp)]
    if(length(tmp)>1 || length(tmp)==0){
      tmp = "A/B"
    }
    tmp = sprintf("%s",tmp)
  } else {
    tmp = "A/B"
  }
  return(tmp)
}
gt.HP.a = unlist(apply(marker.composition[,sample.HP.a],1, FUN = gt.check))
gt.HP.b = unlist(apply(marker.composition[,sample.HP.b],1, FUN = gt.check))
write.table(cbind(gt.HP.a,gt.HP.b), col.names = FALSE, row.names = FALSE, quote = FALSE, file = "tmp.txt", sep = "\t")
save(gt.HP.a, file = "predicted_gt_HP-a_based_on_snp")


#Prepar matrix for Clustring of HP-a
# sample.HP.a = c(sample.HP.a, sample.HP.b)
gt.mat.HP.a = marker.composition[,sample.HP.a]
gt.mat.HP.a = cbind(gt.mat.HP.a,cssl.gt.info[,"HP.a"])
gt.mat.HP.a = cbind(gt.mat.HP.a,cssl.gt.info[,"Takanari"])
gt.mat.HP.a = cbind(gt.mat.HP.a,cssl.gt.info[,"Koshihikari"])
colnames(gt.mat.HP.a) = c(sample.HP.a,"HP.a","Takanari", "Koshihikari")
gt.mat.HP.a[gt.mat.HP.a=="A"] = 1
gt.mat.HP.a[gt.mat.HP.a=="A/B" | gt.mat.HP.a=="-"] = 0
gt.mat.HP.a[gt.mat.HP.a=="B"] = -1

#Clustring
pdf("clustring_of_HP-A_genotype.pdf")
rd=dist(t(gt.mat.HP.a))
rc=hclust(d=rd,method="ward.D2")
plot(rc)
rc=hclust(d=rd,method="average")
plot(rc)
dev.off()

#Plot mapping results of HP-a genome
gt.mat.HP.a.col = gt.mat.HP.a
gt.mat.HP.a.col[gt.mat.HP.a.col==1] = "blue"
gt.mat.HP.a.col[gt.mat.HP.a.col==-1] = "red"
gt.mat.HP.a.col[gt.mat.HP.a.col==0] = "white"
sample.name = c("20091(1)", "20097(2)", "21048(7)", "21054(8)", "HP-a", "20381(3)",  "20485(4)","20551(5)", "20623(6)", "Takanari","Koshihikari")
sample.name.short = c("20091\n(1)", "20097\n(2)", "21048\n(7)", "21054\n(8)", "HP-a", "20381\n(3)",  "20485\n(4)","20551\n(5)", "20623\n(6)", "Tak","Kos")
pdf("HP-a-marker-map.pdf")
plot(1,1, type = "n", xlim = c(1,141), ylim = c(1,11), yaxt = "n", xlab = "Marker", ylab = "LineName")
axis(side=2, at=1:11, labels=sample.name.short)
for(i in 1:ncol(gt.mat.HP.a.col)){
  points(seq(1,141), rep(i,141), pch = "|", col = gt.mat.HP.a.col[,sample.name[i]], cex = 2)
}
dev.off()

#Determine the genotypes of HP-a1 and HP-a2
sample.HP.a.1 = c("20623", "20485", "20551")
sample.HP.a.2 = c("20381", "20097", "21054")

gt.HP.a.1 = unlist(apply(marker.composition[,sample.HP.a.1],1, FUN = gt.check))
gt.HP.a.2 = unlist(apply(marker.composition[,sample.HP.a.2],1, FUN = gt.check))
gt.HP.b = unlist(apply(marker.composition[,sample.HP.b],1, FUN = gt.check))
save(sample.HP.a.1, sample.HP.a.2, gt.HP.a.1, gt.HP.a.2, file = "predicted_gt_HP-a12b_based_on_snp")
write.csv(cbind(gt.HP.a.1,gt.HP.a.2,gt.HP.b), file = "predicted-HP-genotype.csv")
