
# Genera la informaci?n de Demandas
# datalogit: es un objeto procesado por mlogit.data
# logit.tranf: Especifica si se va a graficar el ms o el logit(ms)
# px.rel: Especifica si se van a graficar los precios relativos o absolutos
# alter.set: vector con los nombres de las alternativas que ser?n procesadas. Si no se especifica van todas.
# Esta funcion la usa get.summary.DCM
tmp_get.data.Dx<-function(datalogit, logit.tranf=T, px.rel=T,choice.var="choice", cons.var="IdEncuestado", chid.var="chid", alt.var="IdAlternativa", px.var="precio", alt.ref=length(unique(datalogit[,alt.var]))) {
  #browser()
  # Defino las alternativas a utilizar
  nalter<-length(unique(datalogit[,alt.var]))
  alt.levels<-levels(factor(datalogit[,alt.var]))

  # Guardo los datos de la 1er alternativa
  temp<-datalogit[which(datalogit[,alt.var]==alt.levels[1]),]
  #print(temp)
  t<-tapply(temp[,choice.var],temp[,px.var],mean)
  t<-t[which(!is.na(t))]
  n<-tapply(temp[,choice.var],temp[,px.var],length)

  dataplot<- data.frame(n=n, px.abs=as.numeric(names(t)),px.rel=as.numeric(names(t))/mean(as.numeric(names(t))),dx.data=as.vector(t),logit.dx.data=0,alt=1)

  # Guardo los datos del resto de las alternativas
  for (i in 2:nalter) {

    # print(paste("Calculando alternativa ",i,sep=""))
    temp<-datalogit[which(datalogit[,alt.var]==alt.levels[i]),]
    t<-tapply(temp[,choice.var],temp[,px.var],mean)
    t2<-as.numeric(names(t))/mean(as.numeric(names(t)))
    n<-tapply(temp[,choice.var],temp[,px.var],length)
    new.px.abs<-c(dataplot$px.abs,as.numeric(names(t)))
    new.dx.data<-c(dataplot$dx.data,as.vector(t))
    new.px.rel<-c(dataplot$px.rel,t2)
    new.IdAlternativa<-c(dataplot[,"alt"],rep(i,length((as.vector(t)))))
    new.n<-c(dataplot$n,n)
    #browser()
    dataplot<- data.frame(n=new.n, px.abs=new.px.abs,px.rel=new.px.rel,dx.data=new.dx.data,logit.dx.data=rep(0,length(new.px.abs)),alt=new.IdAlternativa)
  }

  #dataplot$logit.dx.data<-log(dataplot$dx.data/(1-dataplot$dx.data))
  dataplot$logit.dx.data<-log(dataplot$dx.data/(1-dataplot$dx.data))
  #browser()

  # C?lculo de las utilidades
  # px.ref.min<-min(dataplot[dataplot$alt==alt.ref,"px.abs"])
  # p0<-dataplot[dataplot$alt==alt.ref & dataplot$px.abs==px.ref.min,"dx.data"]
  # dataplot$util<-log(dataplot$dx.data/p0)
  dataplot$util<-log(dataplot$dx.data/(1-dataplot$dx.data))


  #   C?lculo de las utilidades
  #   px.ref.min<-min(datalogit[datalogit[,alt.var]==alt.levels[alt.ref],px.var])
  #   for (fila  in 1:(dim(dataplot)[1])) {
  #     alt<-dataplot$alt[fila]
  #     px<-dataplot$px.abs[fila]
  #     chids<-datalogit[datalogit[,alt.var]==alt.levels[alt] & datalogit[,px.var]==px,chid.var]
  #     temp<-datalogit[datalogit[,chid.var] %in% chids,]
  #     #temp<-temp[temp[,alt.var]==alt.levels[alt.ref],choice.var]
  #     temp<-temp[temp[,alt.var]==alt.levels[alt.ref] & temp[,px.var]==px.ref.min,choice.var]
  #     dataplot$util[fila] <- log(dataplot$dx.data[fila]/mean(temp))
  #   }

  elasticidades<-data.frame(alt=alt.levels,e.data=rep(NA,nalter))

  for (i in 1:nalter) {
    data<-subset(dataplot,alt==i)
    ms2<-max(data$dx.data)
    ms1<-min(data$dx.data)
    p2<-max(data$px.abs)
    p1<-min(data$px.abs)
    elasticidades$e.data[i]<-((ms2-ms1)/((ms2+ms1)/2))/((p2-p1)/((p2+p1)/2))
  }

  result<- list()
  result$demandas<-dataplot
  result$elasticidades<-elasticidades
  result
}
