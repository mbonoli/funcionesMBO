tmp_getProductos<- function (datos, prod) {
  datos2<-datos
  datos2$compro<-1
  temp<-tapply(datos2$Primero,list(datos$IdEncuestado,datos$IdAlternativa),sum)
  temp[is.na(temp)]<-0
  temp<-temp[,-which(colnames(temp)==999)]
  colnames(temp)<-prod
  temp
}
