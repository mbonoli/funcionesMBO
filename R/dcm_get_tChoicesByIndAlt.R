dcm_get_tChoicesByIndAlt <- function(datalong,
                                     choice.var="choice", ind.var="ind",
                                     alt.var="alt", drop.alt=NULL,
                                     task.filter=NULL, subtask.filter=NULL, task.var="task", subtask.var="subtask",
                                     type=c("freq", "consid"),
                                     output=c("table","df"),
                                     add.vars=NULL,
                                     drop.alt.wo.choices=T){
  require(dplyr)
  if (length(output)>1) output <- "table"
  dl <- datalong[datalong[,choice.var]==1,]
  if (!is.null(task.filter)){
    dl <- dl[dl[,task.var] %in% task.filter,]
  }
  if (!is.null(subtask.filter)){
    dl <- dl[dl[,subtask.var] %in% subtask.filter,]
  }

  if (!is.null(drop.alt)){
    dl <- dl[!(dl[,alt.var] %in% drop.alt),]
  }
  ftable <- table(dl[,ind.var], dl[,alt.var])

  if (type=="freq"){
    result <- ftable
  } else if (type=="consid"){
    ftable[ftable>0] <- 1
    result <- ftable
  }

  if (drop.alt.wo.choices) {
    cs <- colSums(result)
    result <- result[,cs>0]
  }

  if (!is.null(add.vars)){
    add <- distinct(datalong[,c(ind.var, add.vars)])
    fqtab <- cbind(rownames(result),as.data.frame.matrix(result))
    colnames(fqtab)[1] <- ind.var
    # print(head(fqtab))
    # print(head(add))
    result <- merge(x=add, y=fqtab, by=ind.var)
  } else if (output=="df") {
    raux <- as.matrix.data.frame(result)
    colnames(raux)<-colnames(result)
    rownames(raux)<-rownames(result)
    result <- raux
  }
  result
}
