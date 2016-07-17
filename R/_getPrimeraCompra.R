tmp_getPrimeraCompra<- function (datos, prod) {
  temp<-tapply(datos$Primero,list(datos$IdEncuestado,datos$IdAlternativa),sum)
  temp[is.na(temp)]<-0
  temp<-temp[,-which(colnames(temp)==999)]
  colnames(temp)<-prod
  temp
}
