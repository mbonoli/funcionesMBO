dcm_plot_corr <- function(datalong, alt.var="prod",
                          cor.type=c("spearman", "tetrac"),
                          plot.type=c("circle", "hm"),
                          circle.col=c("red", "blue"),
                          drop.alt=NULL, task.filter=NULL,
                          task.var="task",
                          cex=1, title="",
                          order=c("original", "AOE", "FPC", "hclust", "alphabet")){
  require(corrplot)
  print("Spearman se hace con Frecuencias de compra.")
  print("Tetracoricas se hacen con Conideration Sets.")
  plot.type <- match.arg(plot.type)
  order <- match.arg(order)
  cor.type <- match.arg(cor.type)
  if (cor.type=="spearman"){
    tab <-
      dcm_get_tChoicesByIndAlt(datalong, type = "freq", alt.var="prod",
                               task.filter = task.filter, task.var=task.var,
                               drop.alt=drop.alt, output = "df")
    cordata<-cor(tab, method="spearman",use="complete.obs")
  } else {
    tab <-
      dcm_get_tChoicesByIndAlt(datalong, type = "consid", alt.var="prod",
                               task.filter = task.filter,
                               drop.alt=drop.alt, output = "df")
    cordata<-tetrachoric(tChoicesByIndAlt_COL[,-c(1:2)],correct=TRUE,smooth=TRUE,global=TRUE)$rho
  }
  if (plot.type=="circle"){
    corrplot(cordata, method="circle",
             col = circle.col, tl.cex=cex, outline = F,
             order = order,
             cl.pos="n", main=title)
  }
  if (plot.type=="hm"){
    col3 <- colorRampPalette(c("red", "white", "blue"))
    corrplot(cordata, method="color",
             col = rb, tl.cex=cex, outline = F,
             order = order,
             cl.pos="n", main=title)
  }
}
