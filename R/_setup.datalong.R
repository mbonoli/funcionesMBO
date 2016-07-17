tmp_setup.datalong<-function(datalong, valid.cons, valid.sv=1:20, generar.dummies=T) {
  #browser()

  #Quito 999
  result<-datalong[datalong$IdAlternativa!=999,]

  #Selecciono Subviajes
  result<-datalong[datalong$IdSubviaje %in% valid.sv,]

  #Selecciono Consumidores
  result<-result[datalong$cons %in% valid.cons,]

  #alter-no tiene sentido porque ya viene en la columna cod del dise?o
  #result$alter<-paste("B",prefijos0(result$IdAlternativa),sep="")

  #choice
  result$choice<-(result$Cantidad==1)

  #chid
  result$consv<-paste(prefijos0(result$cons),prefijos0(result$IdViaje),prefijos0(result$IdSubviaje),sep=".")
  result<-sqldf("select * from result order by consv, IdAlternativa")
  ntimes<-sqldf("select count(consv) from result group by consv order by consv")
  result$chid<-rep(1:(length(unique(result$consv))),times=ntimes[,1])

  #Elimino los chid que no tienen compras
  df<-as.data.frame(result)
  tmp<-sqldf("select chid, sum(choice) from df group by chid")
  valid.chid<- tmp[which(tmp[,2]==1),1]
  result<-result[which(result$chid %in% valid.chid),]
  ntimes<-sqldf("select count(consv) from result group by consv order by consv")
  result$chid<-rep(1:(length(unique(result$consv))),times=ntimes[,1])

  #Betas
  if (generar.dummies) result<-put.betas(result,nalt=length(unique(result$alter)),alt.var="alter")

  result
}
