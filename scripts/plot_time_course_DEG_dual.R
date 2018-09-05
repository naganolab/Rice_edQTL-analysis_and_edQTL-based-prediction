plot.time.course.DEG = function(line,des, FDR3,ave1,sd1,ave2,sd2, range,gene){
  plot(as.integer(colnames(ave1)),ave1[gene,], ylim = c(log2(0.1),(range[2]-log2(0.1))*1.3+log2(0.1)), ylab = "log2(RPM+0.1)", xlab="Min. after light induction")
  points(as.integer(colnames(ave1)),ave1[gene,], col = "blue", pch = 16)
  points(as.integer(colnames(ave2)),ave2[gene,], col = "red" , pch = 16)
  lines(as.integer(colnames(ave1)),ave1[gene,], col = "blue")
  lines(as.integer(colnames(ave2)),ave2[gene,], col = "red")
  arrows(as.integer(colnames(ave1)),ave1[gene,]-sd1[gene,],as.integer(colnames(ave1)),ave1[gene,]+sd1[gene,],angle = 90, length = 0.05)
  arrows(as.integer(colnames(ave1)),ave1[gene,]+sd1[gene,],as.integer(colnames(ave1)),ave1[gene,]-sd1[gene,],angle = 90, length = 0.05)
  arrows(as.integer(colnames(ave2)),ave2[gene,]-sd2[gene,],as.integer(colnames(ave2)),ave2[gene,]+sd2[gene,],angle = 90, length = 0.05)
  arrows(as.integer(colnames(ave2)),ave2[gene,]+sd2[gene,],as.integer(colnames(ave2)),ave2[gene,]-sd2[gene,],angle = 90, length = 0.05)
  
  flag = (FDR3[gene,] ==1)
  if(sum(flag)>0){
    text(as.integer(colnames(ave1)[flag]),((ave1[gene,]+ave2[gene,])/2)[flag], "*", col = "green", cex = 1.5)
  }
  title(main = sprintf("%s",gene), sub = des[gene,]$RAP.DB.description)
  legend("topright", cex = 0.5,pch = c(16,16,8), legend=c("kos","tak","DEGs between kos and tak (FDR<0.05)"), col = c("blue","red","green"))
}
