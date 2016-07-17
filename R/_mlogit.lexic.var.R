tmp_mlogit.lexic.var <- function(datalong, var, sentido=c("min", "max"), alt.var, alts,  ind.var, task.var, choice.var){

  require(data.table)
  d <- data.table(datalong)

  # Elimino las altrnativas que no se deben considerar
  valid.rows <- d[,get(alt.var)] %in% alts
  d <- d[valid.rows]

  # Calculo minimo y maximo por ind-task y se lo agrego al datalong
  s <- d[,list(max=max(get(var)),min=min(get(var))),by=eval(paste(ind.var,task.var,sep=","))]
  dl <- merge(d, s, by=c(ind.var,task.var))

  # Me quedo solo con los choice en el datalong
  dl <- dl[dl[,get(choice.var)]==1,]

  # Agrego dos columnas que indican si seleccione el máximo o el mínimo
  dl$minchoice <- as.numeric(dl[,get(var)]==dl[,min])
  dl$maxchoice <- as.numeric(dl[,get(var)]==dl[,max])

  # Selecciono los individuos
  indiv <- dl[,list(nmin=sum(minchoice),nmax=sum(maxchoice),ntask=sum(get(choice.var))),by=ind.var]
  if (sentido=='min'){
    indiv.lexic <- as.numeric(indiv[indiv$nmin==indiv$ntask,get(ind.var)])
  } else {
    indiv.lexic <- as.numeric(indiv[indiv$nmax==indiv$ntask,get(ind.var)])
  }
  cat(paste0("Cantidad de individuos: ",length(indiv.lexic),"\n\n"))
  indiv.lexic
}
