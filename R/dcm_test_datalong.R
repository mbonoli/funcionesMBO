dcm_check_datalong <- function(datalong, ind.var="ind",
                               task.var="task", subtask.var="subtask",
                               chid.var="chid",
                               alt.var="alt", choice.var="choice"){
  cat(paste0(dim(datalong)[1]," filas\n"))
  cat(paste0(dim(datalong)[2]," columnas\n\n"))
  cat(paste0(length(unique(datalong[,ind.var]))," individuos\n"))
  cat(paste0(length(unique(datalong[,chid.var]))," tareas totales\n"))
  taskdata <- distinct(datalong, get(ind.var, envir = as.environment(datalong)),
                       get(task.var, envir = as.environment(datalong)))

  cat("\nTareas por individuo\n")
  taskXind <- distinct(datalong,
                       ind=get(ind.var, envir = as.environment(datalong)),
                       task=get(task.var, envir = as.environment(datalong)))
  tXi <- as.data.frame(table(summarize(group_by(taskXind, ind), freq=n())$freq))
  names(tXi) <- c("Tareas", "Nro indiv")
  print(tXi)

  cat("\nChoice Por Tarea\n")
  choiceXchid <- summarize(group_by(datalong, chid=get(chid.var, envir = as.environment(datalong))),
                           freq=sum(choice))
  # chid=get(chid.var, envir = as.environment(datalong)),
  # choice=sum(get(choice.var, envir = as.environment(datalong))))
  cXc <- as.data.frame(table(summarize(group_by(choiceXchid, chid), freq=n())$freq))
  names(cXc) <- c("Nros ChoiceXtarea", "Nro tareas")
  print(cXc)
  # dcm_test_datalong(datalong)
}
