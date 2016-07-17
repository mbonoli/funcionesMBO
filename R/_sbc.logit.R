dcm_gof_sbc <- function (model){
  k<-length(model$coefficients)
  ll<-model$logLik[1]
  nch<-length(unique(attr(model$model,"index")$chid))
  sbc<- -2*ll+k*log(nch)
  sbc
}
