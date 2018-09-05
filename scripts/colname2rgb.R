colnames2rgb = function(name,alpha){
  rgb = as.matrix(col2rgb(name))/255
  rgb(rgb[1,],rgb[2,], rgb[3,],alpha/100)
}