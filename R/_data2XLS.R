tmp_data2XLS<-function(data){
  #browser()
  datalist<-list(data$cs,data$frecuencias,data$productos,data$demanda,data$elasticidades,data$residuos)
  sheets<-c("ChoiceShare","FrecuenciasCompra","ProductosComprados","Demandas","Elasticidades","Residuos")
  if (!is.null(data$CoefTable)){
    datalist<-list(data$cs,data$frecuencias,data$productos,data$demanda,data$elasticidades,data$residuos,data$CoefTable)
    sheets<-c("ChoiceShare","FrecuenciasCompra","ProductosComprados","Demandas","Elasticidades","Residuos","CoefTable")
  }
  if (!is.null(data$betas.indiv)){
    datalist<-list(data$cs,data$frecuencias,data$productos,data$demanda,data$elasticidades,data$residuos,data$CoefTable,data$betas.indiv)
    sheets<-c("ChoiceShare","FrecuenciasCompra","ProductosComprados","Demandas","Elasticidades","Residuos","CoefTable","Betas.Indiv")
  }
  name<-paste(deparse(substitute(data)),format(Sys.time(), "%Y_%m_%d %Hh%Mm%Ss"),sep=" ")
  createXLS(datalist,sheets,name)
}
