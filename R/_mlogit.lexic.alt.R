tmp_mlogit.lexic.alt <- function(datalong,  alt.var, alts,  ind.var, task.var, choice.var){

  require(data.table)
  d <- data.table(datalong)

  ind.list <- c()

  for (alt in alts){
    # Me quedo solo con los choice=1 en el datalong
    dl <- d[d[,get(choice.var)]==1,]

    dl$altobj <- as.numeric(dl[,get(alt.var)]==alt)
    indiv <- dl[,list(naltobj=sum(altobj),ntask=sum(get(choice.var))),by=eval(ind.var)]

    # Selecciono los individuos
    indiv.lexic <- as.numeric(indiv[indiv$naltobj==indiv$ntask,get(ind.var)])
    ind.list <- c(ind.list,indiv.lexic)
  }
  cat(paste0("Cantidad de individuos: ",length(ind.list),"\n\n"))
  ind.list
}
