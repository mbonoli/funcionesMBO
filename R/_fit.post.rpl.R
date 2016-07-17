# Supone que todos los par?mentros son aletorios
# Calcula un dise?o completo usando los bi correspondientes para cada individuo
tmp_fit.post.rpl<-function (model, design, betas.post) {
  #browser()

  #freq.by.cons<-table(attr(design,"index")$id)
  param<-summary(model)$CoefTable[,1]
  nparam<-length(param)
  param.names<-names(param)[1:(nparam/2)]
  #ncons<-length(unique(attr(design,"index")$id))
  cons<-unique(design$cons)

  chids.by.cons <- c()
  for (i in cons) {
    chids.by.cons<-c(chids.by.cons,length(unique(design[design$cons==i,"chid"])))
  }

  mat.betas.post<-matrix(ncol=nparam/2,nrow=nchid)
  colnames(mat.betas.post)<-param.names
  for (j in param.names) {mat.betas.post[,j]<-rep(betas.post[,j],times=chids.by.cons)}

  p<- fit.rpl(mat.betas.post, design, return.matrix=T)
}
