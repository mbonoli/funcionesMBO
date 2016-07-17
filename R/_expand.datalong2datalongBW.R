# Esta funci?n convierte un datalong con varios choices en distintas columas
# en un unico datalong con todos los subviajes.
# LE FALTA AGREGAR EL TEMA BWBW POR AHORA SOLO BBBB
tmp_expand.datalong2datalongBW<-function (datalong, choices.var) {

  nsubviajes<-length(choices.var)

  # Armo el primer bloque
  data<-datalong[,!(colnames(datalong) %in% choices.var)]
  delete<-matrix(ncol=nsubviajes,nrow=dim(data)[1])

  # Le agrego la variable choice
  data$choice<-(datalong[,choices.var[1]]==data$alt)
  delete[,1]<-0
  delete[,2]<-data$choice
  data$delete<-delete[,1]
  data$subviaje<-paste(prefijos0(data$viaje,2),".1",sep="")

  for (i in 2:length(choices.var)) {
    data2<-datalong[,!(colnames(datalong) %in% choices.var)]
    data2$choice<-(datalong[,choices.var[i]]==data2$alt)
    data2$delete<-delete[,i]
    data2$subviaje<-paste(prefijos0(data2$viaje,2),prefijos0(i),sep=".")
    if (i<length(choices.var)) {delete[,i+1]<-delete[,i]+data2$choice}
    data<-rbind(data,data2)
  }

  #Le quito las filas que no ve el consumidor
  data<-subset(data,delete==0)
  # Ordeno el dataframe
  data<-data[order(data$cons,data$viaje,data$subviaje),]

  # Cambio los valores de la columna chid
  data$cvs<-paste(prefijos0(data$cons),data$subviaje,sep=".")
  nviajes<-length(unique(data$cvs))
  data$chid<-rep(1:nviajes,table(data$cvs))

  # Le quito la columna delete
  data<-data[,(colnames(data)!="delete")]
  # Borro los nombres de las filas porque sino quedan duplicadas
  rownames(data)<-NULL

  data[,colnames(data)!="cvs"]
}
