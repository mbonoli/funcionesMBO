tmp_plot.data <- function (
  data,
  logit.tranf=F,
  px.rel=F,
  plot.post=T,
  alt.no.plot=NA,
  legend.multDx.size=.8,
  cor.method="spearman") {
  aux<-data
  attach(aux,warn.conflicts = F)

  nalt<-data$nalt

  # Bar plot Choice Share
  windows()
  barplot(cs,
          main="Choice Share Por Alternativa",
          names.arg=data$alt.names,
          cex.names=0.8,
          las=2)

  # Correlaciones Frecuencias Empiricas
  windows()
  require(psych)
  cordata<-cor(frecuencias,method=cor.method,use="complete.obs")
  colnames(cordata)<-rownames(cordata)<-data$alt.names
  cor.plot(cordata,numbers=F,
           main=paste("Correlaciones Frecuencias Empíricas (",cor.method,")"))

  windows()
  cordata <- tetrachoric(data$productos,correct=TRUE,smooth=TRUE,global=TRUE)$rho
  cor.plot(cordata,numbers=F,main="Correlaciones Consideration Sets (Tetracoricas)")

  # mds scale
  windows()
  d<-dist(t(productos),method="binary")
  # d<-dist(t(data$frecuencias),method="binary")
  mds1 = cmdscale(d, k=2)
  plot(mds1[,1], mds1[,2], type = "n", xlab = "", ylab = "", axes = FALSE,
       main = "cmdscale (stats)")
  text(mds1[,1], mds1[,2], alt.names, cex=0.9)

  # prepare hierarchical cluster
  windows()
  hc = hclust(dist(cordata))
  # very simple dendrogram
  plot(hc)

  # Gráfico de todas las demandas juntas
  if (is.px.var) {

    windows()
    dataplot<-demandas

    if (logit.tranf==T) {y<-"util"} else {y<-"dx.data"}
    if (px.rel==T) {x<-"px.rel"} else {x<-"px.abs"}

    # get the range for the x and y axis
    xrange <- range(dataplot[x])
    xrange <- c(xrange[1], xrange[2]*1.15)   # para dejar lugar para las leyendas
    yrange <- range(dataplot[,y],finite=T)

    plot(xrange, yrange, type="n", xlab=x, ylab=y )
    colors <- rainbow(nalt)
    linetype <- c(1:nalt)
    plotchar <- seq(18,18+nalt,1)
    for (i in 1:nalt) {
      if (is.na(alt.no.plot[1]) | !(i %in% alt.no.plot)) {
        dataplot <- subset(demandas, alt==i)
        dataline<-dataplot[,which(names(dataplot) %in% c(x,y))]
        dataline<-dataline[!is.infinite(dataline[,y]),] # Quito los infinitos por ln(0)
        lines(lowess(dataline[,1],dataline[,2],f=2), lwd=2.5, lty=linetype[i], col=colors[i], pch=plotchar[i])
      }
    }

    # add a title and subtitle
    title("Curvas de Demanda")
    # add a legend
    if (is.na(data$alt.names[1])) {
      legend(xrange[2]/1.15, yrange[2], alts, cex=legend.multDx.size, col=colors,
             lty=linetype, title="Prod", lwd=2.5)}
    else {
      legend(xrange[2]/1.15, yrange[2], data$alt.names[!( 1:nalt %in% alt.no.plot)], cex=legend.multDx.size, col=colors,
             lty=linetype, title="Prod", lwd=2.5)}
  }

  # Gr?ficos de los betas del modelo
  if (!is.null(data$CoefTable)) {
    windows()
    model.nalt<-nrow(data$CoefTable)/2
    li<-data$CoefTable[1:model.nalt,1]-2*data$CoefTable[(model.nalt+1):(2*model.nalt),1]
    ls<-data$CoefTable[1:model.nalt,1]+2*data$CoefTable[(model.nalt+1):(2*model.nalt),1]
    sig<-data$CoefTable[1:model.nalt,4]
    sig.sd<-data$CoefTable[(model.nalt+1):(2*model.nalt),4]
    plot(range(c(li,ls)),c(1,model.nalt),type="n",xlab="Distribuci?n de los Betas",yaxt="n", ylab="")
    for(s in 1:model.nalt){
      if (sign(li[s])!=sign(ls[s])){color="black"}else{color="red"}
      lty="solid"
      if (sig[s]<.01){lwd=2}
      else{
        lwd=1
        if (sig[s]>.1) {
          if (sig.sd[s]>.05) {lty="dotted"}
          else {lty="dashed"}
        }
      }
      #if (s==6) browser()
      segments(li[s],model.nalt-s+1,ls[s],model.nalt-s+1,col=color,lwd=lwd,lty=lty)
    }
    abline(v=0)
    axis(2, model.nalt:1, rownames(data$CoefTable)[1:model.nalt],las=1)
  }


  # GRAFICO DE LAS CURVAS DE DEMANDA POR SEPARADO
  browser()
  if (is.px.var) {

    dx.table <- demandas


    tmp.y<-dx.table$dx.data
    if (!is.null(dx.table$dx.model)) {tmp.y<-c(tmp.y,dx.table$dx.model)}
    if (!is.null(dx.table$dx.model.post)) {tmp.y<-c(tmp.y,dx.table$dx.model.post)}
    y.range<-range(tmp.y[!is.na(tmp.y)])

    cont<-10
    for (i in 1:nalt) {
      if (cont==10) {
        windows()
        par(mfrow=c(2,5))
        cont<-1
      }
      else {cont <- cont+1}
      dataplot <- subset(dx.table, alt==i)
      x.range<-range(dataplot$px.abs)
      plot(x.range, y.range, type="n",main =paste(names(mt.asv[which(mt.asv[,i]==1),1]),collapse="-"),xlab=paste("Px B",i,sep=""),ylab="pm")
      plot(x.range, y.range, type="n",xlab=ifelse(is.na(data$alt.names[i]),paste("Px ",alts[i],sep=""),paste("Px \n",data$alt.names[i],sep="")),ylab="pm")
      points(dataplot$px.abs,dataplot$dx.data, col = "black", cex = 1.5)

      if (!is.null(dx.table$dx.model)) {lines(lowess(dataplot$px.abs,dataplot$dx.model,f=3),col="blue",lty="dashed")}
      if (plot.post) {lines(lowess(dataplot$px.abs,dataplot$dx.post,f=3),col="blue")}
    }
  }

}
