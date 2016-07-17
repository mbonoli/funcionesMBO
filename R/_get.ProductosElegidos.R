tmp_get.ProductosElegidos <- function (data.clean,name)  {
  result<-table(data.clean$IdEncuestado,data.clean$IdAlternativa)
  result<-	result [,1:(ncol(result)-1)]  #quito la col de 999
  result<- (result>0)+0
  print ("Frecuencias de compra (%)")
  print(colSums(result)/sum(result)*100 )
  result [,1:(ncol(result)-1)]
}
