# Esta funci?n calcula las probabilidades de un modelo rpl
# Entro con un dise?o que tiene todas las variables + 2 columnas: chid y alt
# Entro con una matriz de coeficientes que tiene los betas y un rengl?n por chid
# El n?mero de chids de la matriz de dise?o corresponde al n?mero de filas de la matriz de betas
tmp_fit.rpl<- function (mat.betas, mat.design, return.matrix=F) {
  #browser()
  X<-mat.design[,!colnames(mat.design) %in% c("chid","alt","choice","cons")]
  chids.lengths<-table(as.numeric(mat.design$chid)) # as.numeric es para que no lo considere como chr y los ordene

  B<-matrix(ncol=ncol(X),nrow=nrow(X))
  for (j in 1:(ncol(X))) {
    B[,j]<-rep(mat.betas[,j],times=chids.lengths)
  }

  util<- B * X
  util$suma.util<-rowSums(util)
  util$exp.suma.util<-exp(util$suma.util)
  util$chid<-as.numeric(mat.design[,"chid"])
  util$alt<-mat.design[,"alt"]
  sumas.by.chid<-tapply(util$exp.suma.util,util$chid,sum)
  util$tot.exp.util<-sumas.by.chid[as.character(util$chid)]
  util$pij<-util$exp.suma.util/util$tot.exp.util
  #browser()

  if  (!return.matrix){
    result<-tapply(util$pij,util$alt,mean)}
  else  {
    result<-util[,c("chid","alt","pij")]}
  result
}
