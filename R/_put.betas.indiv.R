tmp_put.betas.indiv<-function(model, data,datalogit, R.bi=500) {
  #  browser()
  betas.indiv<-getBetasIndividuales (model=model, datalong=datalogit, choice.var=data$choice.var, cons.var=data$cons.var, chid.var=data$chid.var, alt.var=data$alt.var, R.bi=R.bi)
  data$betas.indiv<-betas.indiv
  data$data.post<-TRUE
  data
}
