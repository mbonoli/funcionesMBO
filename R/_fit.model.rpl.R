#Supone que todos los par?mentros son aletorios
tmp_fit.model.rpl<-function (model, design, design.nsim=1) {
  #browser()
  nalt<-length(unique(design$alt))
  param<-summary(model)$CoefTable[,1]
  nparam<-length(param)
  medias<-param[1:(nparam/2)]
  desvios<-param[(nparam/2+1):nparam]
  chids<-unique(design$chid)
  nchid<-length(unique(design$chid))

  simul.betas<-matrix(ncol=nparam/2,nrow=nchid)
  for (i in 1:nchid) {
    simul.betas[i,]<-rnorm(nparam/2)*desvios+medias
  }

  # P es una matriz donde se guardan las distintas estimaciones luego de i iteraciones para analizar convergencia
  p<-matrix(0,nrow=design.nsim,ncol=nalt)
  p[1,]<- fit.rpl(simul.betas, design)
  if (design.nsim>1){
    for (i in 2:design.nsim) {
      p[i,]<- (p[i-1,]*(i-1)+fit.rpl(simul.betas, design))/i
    }
  }

  #names(p)<-rownames(design)
  p[design.nsim,]
}
