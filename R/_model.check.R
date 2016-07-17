# Chequea la estabilidad de un modelo
# Si se entra solo un modelo, se corre de nuevo el mismo modelo y se comparan las diferecias
tmp_model.check<- function (model, model2=NA) {
  #browser()
  nvar<-length(model$coefficients)
  if (is.na(model2)[1]) {model2<-eval(model$call)}
  result<-matrix(nrow=nvar/2,ncol=6)
  result[,1]<-model$coefficients[1:(nvar/2)]
  result[,2]<-model2$coefficients[1:(nvar/2)]
  result[,3]<-floor(100*(result[,1]/result[,2]-1)*100)/100
  result[,4]<-model$coefficients[(nvar/2+1):nvar]
  result[,5]<-model2$coefficients[(nvar/2+1):nvar]+.1
  result[,6]<-floor(100*(result[,4]/result[,5]-1)*100)/100
  #browser()
  rownames(result)<-names(model2$coefficients[1:(nvar/2)])
  colnames(result)<-c("mu1","mu2","Dmu%","sigma1","sigma2","Dsigma%")
  result
}
