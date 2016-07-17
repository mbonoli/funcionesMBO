dcm_add_chid <- function(datalong, indvar, taskvar, subtaskvar=NULL, chidvarname="chid"){
  if (is.null(subtaskvar)){
    datalong <- datalong[order(datalong[,indvar],datalong[,taskvar]),]
    chidkey <- table(paste0(prefix0(datalong[,indvar],5),"-",prefix0(datalong[,taskvar],5)))
    datalong[,chidvarname] <- rep(1:(length(chidkey)), chidkey)
  } else {
    datalong <- datalong[order(datalong[,indvar],datalong[,taskvar],datalong[,subtaskvar]),]
    chidkey <- table(paste0(prefix0(datalong[,indvar],5),
                            "-",prefix0(datalong[,taskvar],5),
                            "-",prefix0(datalong[,subtaskvar],5)))
    datalong[,chidvarname] <- rep(1:(length(chidkey)), chidkey)
  }
  datalong
}

