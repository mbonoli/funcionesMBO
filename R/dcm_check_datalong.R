dcm_check_datalong <- function(datalong, ind.var="ind",
                               task.var="task", subtask.var="subtask",
                               chid.var="chid",
                               alt.var="alt", choice.var="choice",
                               return.obj = F){

  result <- list()

  cat(paste0(dim(datalong)[1]," filas\n"))
  cat(paste0(dim(datalong)[2]," columnas\n\n"))
  cat(paste0(length(unique(datalong[,ind.var]))," individuos\n"))
  cat(paste0(length(unique(datalong[,chid.var]))," tareas totales\n"))
  taskdata <- distinct(datalong, get(ind.var, envir = as.environment(datalong)),
                       get(task.var, envir = as.environment(datalong)))

  cat("\nTareas por individuo (sin considerar subtareas)\n")
  taskXind <- distinct(datalong,
                       ind=get(ind.var, envir = as.environment(datalong)),
                       task=get(task.var, envir = as.environment(datalong)))
  tXi <- summarize(group_by(taskXind, ind), freq=n())
  result[["taskXind"]] <- arrange(tXi, freq)
  ftXi <- as.data.frame(table(tXi$freq))
  names(ftXi) <- c("Tareas", "Nro indiv")
  print(ftXi)

  cat("\nChoice Por Chid\n")
  if(any("choice" %in% names(datalong))){
    choiceXchid <- summarize(group_by(datalong, chid=get(chid.var, envir = as.environment(datalong))),
                             freq=sum(choice))
    result[["choiceXchid"]] <- arrange(choiceXchid, freq)
    cXc <- as.data.frame(table(choiceXchid$freq))
    names(cXc) <- c("Nros ChoiceXtarea", "Nro tareas")
    print(cXc)
  } else {
    cat(paste0("!!!! No se encontró la variable '",choice.var,"'.\n"))
  }
    
  cat("\nChids correlativos\n")
  print(ifelse(length(unique(datalong$chid))==max(datalong$chid),"SI","NO"))
  
  if (return.obj) result
}
