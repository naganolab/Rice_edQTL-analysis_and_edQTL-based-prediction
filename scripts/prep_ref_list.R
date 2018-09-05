ref.df = read.delim("input_data/Reference_20180116102811.txt", stringsAsFactors = F)
CGSNL.Gene.Symbol = strsplit(ref.df$CGSNL.Gene.Symbol,",")
CGSNL.Gene.Symbol2 = strsplit(gsub(" ","",ref.df$Gene.Name.Synonym),",")
for(i in 1:length(CGSNL.Gene.Symbol)){
  CGSNL.Gene.Symbol[[i]] = c(CGSNL.Gene.Symbol[[i]], CGSNL.Gene.Symbol2[[i]])
}
CGSNL.Gene.Symbol.uniq = unique(unlist(CGSNL.Gene.Symbol))
ref.list = rep(list(list()), length(CGSNL.Gene.Symbol.uniq))
names(ref.list) = CGSNL.Gene.Symbol.uniq
for(i in 1:length(CGSNL.Gene.Symbol)){
  genes = CGSNL.Gene.Symbol[[i]]
  for(gn in genes){
    ref.list[[gn]] = c(ref.list[[gn]], ref.df$Title[i])
  }
}

RAP.CGSNL = read.delim("input_data/OryzabaseGeneListAll_20180116010000.matrix", header = F, stringsAsFactors = F, sep = "\t")
rownames(RAP.CGSNL) = RAP.CGSNL[,2]
flag = is.element(CGSNL.Gene.Symbol.uniq,RAP.CGSNL[,2])
CGSNL.Gene.Symbol.uniq[flag] = RAP.CGSNL[CGSNL.Gene.Symbol.uniq[flag],1]
RAP.uniq = unique(RAP.CGSNL[,1])
tmp2 = NULL
for(gn in RAP.uniq){
  tmp = RAP.CGSNL[is.element(RAP.CGSNL[,1],c(gn)),2]
  tmp = tmp[is.element(tmp, CGSNL.Gene.Symbol.uniq)]
  if(length(tmp)>0){
    flag = 0
    i = 1
    while(flag==0&&i<=length(tmp)){
      if(length(ref.list[[tmp[i]]])>0){
        flag = 1
      }
      i = i + 1
    }
    if(flag>0){
      tmp2 = c(tmp2,gn)
    }
  }
}
ref.list = tmp2
save(ref.list, file = "input_data/ref_list")