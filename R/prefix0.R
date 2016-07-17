prefix0 <- function(numero, caracteres=NA) {
  long.num<-nchar(numero)
  if (is.na(caracteres)) {caracteres<-max(nchar(numero))}
  if (sum(long.num>caracteres)>0) {
    stop("El numero tiene mas caracteres de lo posible. Ver funcion prefijos0")
  }
  ceros<-NULL
  for (i in 1:caracteres) {ceros<-paste(ceros,"0",sep="")}
  result<-rep(ceros,length(numero))
  prefijos<-substr(result,1,caracteres-long.num)
  result<-paste(prefijos,numero,sep="")

  result
}
