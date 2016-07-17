# Ex clean.cons
tmp_get.valid.cons<- function (cons.behav, IdSubviaje, num.min.viajes,num.min.compras,num.min.alter.dif) {
  # browser()
  data<-cons.behav[[IdSubviaje]]
  result<-sqldf(paste(
    "select cons from data
    where ViajesConCompras+ViajesSinCompras>=",num.min.viajes,"
    and ViajesConCompras>=",num.min.compras,"
    and ProductosDistintosElegidos>=",num.min.alter.dif, sep=""))
  cat (paste("\nN?mero de consumidores totales: ",length(data$cons),sep=""))
  cat (paste("\nN?mero de consumidores descartados: ",length(data$cons)-length(result[,1]),sep=""))
  cat (paste("\nN?mero de consumidores v?lidos: ",length(result[,1]),sep=""))
  result[,1]
}
