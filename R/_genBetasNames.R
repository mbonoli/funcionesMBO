tmp_genBetasNames<-function(j) {
  if (j<10) {
    result<-paste("B0",1:j,sep="")
  }
  else {
    result<-paste("B0",1:9,sep="")
    result<-c(result,paste("B",10:j,sep=""))
  }
  result
}
