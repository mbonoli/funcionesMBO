tmp_expand.multichoice.wide<-function(datalong,choice.var,cons.var="cons",trip.var="viaje",alt.var="alt",chid.var="chid",view.var=NA,px.var="preciobase") {
  #browser()
  c<-0
  for (i in choice.var) {
    c<-c+1
    data <-datalong [,c(-which(names(datalong) %in% choice.var))]
    data$choice <- datalong[,i]
    data$choicenum<-c
    # Elimino todos los choicets que no tienen elecciones
    data<-data[which(data$chid %in% unique(data$chid)[tapply(data$choice,data$chid,sum)==1]),]
    #Elimino lo que no se muestra
    data<-subset(data,!is.na(choice))

    # Elimino los nombres sino no puedo hacer el rbind
    rownames(data)<-NULL
    ifelse (c==1,datafull<-data,datafull<-rbind(datafull,data))
  }

  # Ordeno las columnas
  if (is.na(view.var)) {
    datafull<-datafull[,c(cons.var,trip.var,"choicenum",alt.var,chid.var,px.var,betas,"choice")]}
  else {
    datafull<-datafull[,c(cons.var,trip.var,"choicenum",alt.var,chid.var,view.var,px.var,betas,"choice")]}
  # Ordeno las filas
  datafull <- sort.data.frame(~cons+viaje+choicenum+alt,datafull)
  # Agrego la nueva columna chid
  datafull$chid<-rep(1:(nrow(datafull)/nalt),each=nalt)
  rownames(datafull)<-NULL
  datafull
}
