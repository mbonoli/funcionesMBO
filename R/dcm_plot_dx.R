dcm_plot_dx <- function(datalong, alt.var, altshow=NA,
                        yvar=c("dx", "logdx"),
                        pxvar, choicevar="choice",
                        task.filter=NULL, subtask.filter=NULL,
                        task.var="task", subtask.var="subtask",
                        drop.alts=NULL,
                        return.table=F,
                        title=NULL){
  require(dplyr)
  require(plotly)
  yvar <- match.arg(yvar)
  # Filter
  if (!is.null(task.filter)){
    datalong <- datalong[datalong[,task.var] %in% task.filter,]
  }
  if (!is.null(subtask.filter)){
    datalong <- datalong[datalong[,subtask.var] %in% subtask.filter,]
  }
  if (!is.null(drop.alts)){
    datalong <- datalong[!(datalong[,alt.var] %in% drop.alts),]
  }

  # Summarise
  comm <- paste0("dataplot <- summarise(group_by(datalong, ", alt.var,", ",pxvar,
                 ifelse (is.na(altshow), "", paste0(", ",altshow)), "), ")
  comm <- paste0(comm, "dx=mean(", choicevar,"))")
  eval(parse(text=comm))

  xlist <- list(title="precio")
  ylist <- list(title="% choice")
  if (yvar=="logdx") {
    dataplot[,"dx"] <- log10(dataplot[,"dx"]+.001)
    ylist <- list(title="Log(% choice + .001)")
  }

  p <- plot_ly(data = dataplot, type = "scatter",
                x = get(pxvar), y = dx,
                group=get(alt.var))
  if (!is.null(title)) {
    p <- layout(p, title=title, xaxis=xlist, yaxis=ylist)
  } else {
    p <- layout(p, xaxis=xlist, yaxis=ylist)
  }
  print(p)
  if (return.table) dataplot
}
