tmp_put.models.in.data<-function(model, data,datalogit, model.px.var=data$px.var, put.model=T, put.betas=T, put.post.model=T,R.bi=500,R.model=1) {

  data$CoefTable<-summary(model)$CoefTable
  if(put.model) {
    cat("\n\nAgregando informaci?n del modelo a Priori.\n\n")
    data<-put.model.data(model=model, data=data,datalogit=datalogit, model.px.var=model.px.var,only.sig=T, design.nsim=R.model)
  }
  if(put.betas) {
    cat("\n\nAgregando Betas a Posteriori.\n\n")
    data<-put.betas.indiv(model=model, data=data,datalogit=datalogit, R.bi=R.bi)
  }
  if(put.post.model) {
    cat("\n\nAgregando informaci?n del modelo a Posteriori.\n\n")
    data<-put.post.data(model=model, data=data,betas.post=data$betas.indiv, model.px.var=model.px.var,only.sig=T, R=8000)
  }
  data
}
