tmp_plot.alt.dx.compare <- function(model1.dx, model2.dx) {

  dx.table1 <- model1.dx$demandas
  dx.table2 <- model2.dx$demandas

  nalter <- max(dx.table1$alt)

  tmp.y<-c(dx.table1$dx.data)
  if (!is.null(dx.table1$dx.model)) {tmp.y<-c(tmp.y,dx.table1$dx.model,dx.table2$dx.model)}
  if (!is.null(dx.table1$dx.model.post)) {tmp.y<-c(tmp.y,dx.table1$dx.model.post,dx.table2$dx.model.post)}
  y.range<-range(tmp.y)

  cont<-10
  for (i in 1:nalter) {
    if (cont==10) {
      windows()
      par(mfrow=c(2,5))
      cont<-1
    }
    else {cont <- cont+1}
    data <- subset(dx.table1, alt==i)
    data2 <- subset(dx.table2, alt==i)
    x.range<-range(data$px.abs)
    #plot(x.range, y.range, type="n",main =paste(names(mt.asv[which(mt.asv[,i]==1),1]),collapse="-"),xlab=paste("Px B",i,sep=""),ylab="pm")
    plot(x.range, y.range, type="n",xlab=paste("Px B",i,sep=""),ylab="pm")
    points(data$px.abs,data$dx.data, col = "black", cex = 1.5)

    #if (!is.null(dx.table$dx.model)) {lines(lowess(data$px.abs,data$dx.model,f=3),col="blue")}
    #if (!is.null(dx.table$dx.model.post)) {lines(lowess(data$px.abs,data$dx.model.post,f=3),col="red")}
    lines(lowess(data$px.abs,data$dx.model.post,f=3),col="red")
    lines(lowess(data$px.abs,data2$dx.model.post,f=3),col="blue")
  }
}
