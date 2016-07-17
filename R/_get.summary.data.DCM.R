tmp_get.summary.data.DCM <- function (
  datalong,
  scen.var=NULL,
  trip.var="viaje",
  alt.names=NULL,
  choice.var="choice",
  cons.var="IdEncuestado",
  chid.var="chid",
  alt.var="IdAlternativa",
  view.var="view",
  data.px.var="precio",
  model.px.var="precio",
  is.of.var=F,
  alt.ref=NULL,
  file.name="SummaryDCM") {

  require(xlsx)
  result<-list()
  datalong<-as.data.frame(datalong)

  # Precios Variables
  if (is.na(data.px.var[1])) {is.px.var<-FALSE} else is.px.var<-TRUE
  cat(paste("\n  is.px.var:  ",is.px.var))
  result$is.px.var<-is.px.var

  # Oferta Variable
  if (is.of.var) {is.of.var<-TRUE} else is.of.var<-FALSE
  cat(paste("\n  is.of.var:  ",is.of.var))
  result$is.of.var<-is.of.var

  # Escenarios
  if (!is.na(scen.var)) {
    nscen<-length(scen.var)
    cat(paste("\n  n.scen:   ", nscen))
    cat(paste("\n  n.var:   ", scen.var))
    scen.names<-unique(datalogit[,scen.var])
    cat(paste("\n  scen.names:   ", scen.names))
  }

  #  trip.var
  cat(paste("\n  trip.var:   ", trip.var))
  result$trip.var<- trip.var

  #  choice.var
  cat(paste("\n  choice.var: ", choice.var))
  result$choice.var<- choice.var

  #  cons.var
  cat(paste("\n  cons.var:   ", cons.var))
  result$cons.var<- cons.var

  #  chid.var
  cat(paste("\n  chid.var:   ", chid.var))
  result$chid.var<- chid.var

  #  view.var
  cat(paste("\n  view.var:   ", view.var))
  result$view.var<- view.var

  #  px.var
  cat(paste("\n  data.px.var:     ", data.px.var))
  result$px.var<- data.px.var

  # Data Survey (Esta variable estar? en TRUE al incluir los datos de demanda de la encuesta)
  data.survey<-TRUE
  cat(paste("\n  data.survey:",data.survey))
  result$data.survey<-data.survey

  # Data Model (Esta variable estar? en TRUE al incluir los datos de demnada del modelo)
  data.model<-FALSE
  cat(paste("\n  data.model: ",data.model))

  result$data.model<-data.model

  # Data Post
  data.post<-FALSE
  cat(paste("\n  data.post:  ",data.post))
  result$data.post<-data.post

  # nchid
  nchid<-max(datalong[,chid.var])
  cat(paste("\n  nchid:      ",nchid))
  result$nchid<-nchid

  # chids
  chids<-unique(datalong[,chid.var])
  cat(paste("\n  chids:       ",chids[1],"..",chids[nchid],sep=""))
  result$chids<-chids

  # nalt
  nalt<-length(unique(datalong[,alt.var]))
  cat(paste("\n  nalt:       ",nalt))
  result$nalt<-nalt

  # alt.var
  cat(paste("\n  alt.var:    ", alt.var))
  result$alt.var<- alt.var

  # alts
  alts<-unique(datalong[,alt.var])[order(unique(datalong[,alt.var]))]
  if (is.of.var==T) {alts<-alts[order(alts)]}
  cat(paste("\n  alts:        ",alts[1],"..",alts[nalt],sep=""))
  result$alts<-alts

  # Alt Names
  nalt<-length(unique(datalong[,alt.var]))
  # alt.names<-paste(genBetasNames(nalt), alt.names,sep=".")
  # alt.names<-paste(alt.names,sep=".")
  if (is.null(alt.names)) {alt.names<-paste("B", alts[order(alts)], sep="")}
  cat("\n  alt.names:   ")
  cat(as.character(alt.names))
  result$alt.names<-as.vector(alt.names)

  # ncons
  ncons<-length(unique(datalong[,cons.var]))
  cat(paste("\n  ncons:      ",ncons))
  result$ncons<-ncons

  # cons
  cons<-unique(datalong[,cons.var])
  cat(paste("\n  cons:        ",cons[1],"..",cons[ncons],sep=""))
  result$cons<-cons

  # nviajes
  nviajes<-max(datalong[,trip.var])
  cat(paste("\n  nviajes:    ",nviajes))
  result$nviajes<-nviajes

  # viajes
  viajes<-sort(unique(datalong[,trip.var]))   # El sort() es porque el primer consumidor puede no haber contestado todo y queda al final
  cat(paste("\n  viajes:      ",min(viajes),"..",max(viajes),sep=""))
  result$viajes<-viajes

  # Diseño
  if (is.of.var & !is.px.var & FALSE) {
    design<-list()
    # Sumo el datalogit para tener las visualizaciones por vieje-consumidor
    design.full<-aggregate(as.matrix(datalong[,alts]),list(Viaje=datalong[,trip.var],Cons=datalong[,cons.var]),sum)
    # Convierto los n?meros mayores a 1. Puede haber mayores a 1 por la expansi?n de las elecciones m?ltiples
    design.full<-(design.full[,alts]>=1)+0
    i<-1
    repeat {
      i<-i+1
      if (sum(design.full[1,]==design.full[i,])==nalt) break
    }
    matrix<-design.full[1:(i-1),alts]
    design$matrix<-matrix
    cat("\n ..$design$matrix: Matriz de dise?o")

    design$nrows<-nrow(design$matrix)
    cat(paste("\n ..$design$nrows: ",design$nrows))

    design$presenciaXpresencia<-t(design$matrix) %*% design$matrix
    cat("\n ..$design$presenciaXpresencia: Matriz de presencia vs presencia de alternativas")

    design$presenciaXauscencia<-t(design$matrix) %*% (1-design$matrix)
    cat("\n ..$design$presenciaXauscencia: Matriz de presencia vs auscencia de alternativas")

    result$design<-design
  }

  # %Choice
  if (is.of.var==T) {
    result$cs<-rep(NA,nalt)  # se completa mas adelante
    # falta terminar
    names(result$cs)<-alt.names
    result$cs<-tapply(datalong[,choice.var],datalong[,alt.var],mean)
  } else {
    result$cs<-tapply(datalong[,choice.var],datalong[,alt.var],mean)
    names(result$cs)<-alt.names
  }

  cat(paste("\n ..$cs: Choice Share Alternativas"))

  # frecuencias
  frecuencias<-table(datalong[datalong[choice.var]==T,cons.var],datalong[datalong[,choice.var]==T,alt.var])
  result$frecuencias<-data.frame(unclass(frecuencias))
  #   colnames(result$frecuencias)<-alt.names
  cat("\n ..$frecuencias = Frecuencias compra de Productos para cada individuo")
  if (!is.na(scen.var)) {
    frecuencias.scen<-list()
    result$frecuencias.scen<-list()
    for (i in scen.names) {
      frecuencias.scen[[i]]<-table(datalong[which(datalong[choice.var]==T & datalong[scen.var]==i),cons.var],datalong[which(datalong[choice.var]==T & datalong[scen.var]==i),alt.var])
      result$frecuencias.scen[[i]]<-frecuencias.scen[[i]]
      colnames(result$frecuencias.scen[[i]])<-alt.names
      cat(paste("\n ..$frecuencias.",i," = Frecuencias compra de Productos para cada individuo",sep=""))
    }
  }

  # productos
  productos<-(frecuencias>0)+0
  result$productos<-productos
  colnames(result$productos)<-alt.names
  cat("\n ..$productos = Productos comprados por individuo")

  # Curvas de demandas y elasticidades (solo si no es de oferta variable)
  if (is.px.var==T) {
    if (exists("alt.ref")) {tempdx <- get.data.Dx (datalong, logit.tranf=T, px.rel=T,choice.var=choice.var, cons.var=cons.var, chid.var=chid.var, alt.var=alt.var, px.var=data.px.var, alt.ref=alt.ref)}
    else {tempdx <- get.data.Dx (datalong, logit.tranf=T, px.rel=T,choice.var=choice.var, cons.var=cons.var, chid.var=chid.var, alt.var=alt.var, px.var=data.px.var)}
  }

  if (is.px.var) {
    result$elasticidades<-tempdx$elasticidades
    cat("\n ..$elasticidades = Elasticidades de los alternativas")
    result$demandas<-tempdx$demandas
    cat("\n ..$demanda = Demandas de las alternativas")
    result$px.var<-data.px.var
  }

  write.xlsx(round(data$cs *100,2),
             col.names=F,
             file=paste(file.name,"xlsx",sep="."),
             sheetName="share of choice",
             append=F)
  write.xlsx(data.frame(unclass(data$frecuencias)),
             col.names=T,
             file=paste(file.name,"xlsx",sep="."),
             sheetName="choice empirico",
             append=T)
  write.xlsx(data$productos,
             col.names=T,
             file=paste(file.name,"xlsx",sep="."),
             sheetName="consideration set",
             append=T)
  write.xlsx(data$elasticidades,
             col.names=T,
             file=paste(file.name,"xlsx",sep="."),
             sheetName="elasticidades",
             append=T)
  write.xlsx(data$demandas,
             col.names=T,
             file=paste(file.name,"xlsx",sep="."),
             sheetName="demandas",
             append=T)

  cat("\n")
  result
}
