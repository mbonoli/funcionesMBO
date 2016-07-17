tmp_getUnidades<- function (datos, prod) {
  temp<-tapply(datos$Cantidad,list(datos$IdEncuestado,datos$IdAlternativa),sum)
  temp[is.na(temp)]<-0
  temp<-temp[,-which(colnames(temp)==999)]
  colnames(temp)<-prod
  temp
}
