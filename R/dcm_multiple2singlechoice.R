dcm_multiple2singlechoice<- function (datalong, choicevar="choice", chidvar="chid", taskvar, indvar) {

  require(dplyr)
  names(datalong)[names(datalong)==choicevar] <- "choice"
  names(datalong)[names(datalong)==chidvar] <- "chid"

  choiceXchid <- summarise(group_by(datalong, chid), freq=sum(choice))
  # print(head(choiceXchid))

  chids <- choiceXchid$chid
  nchids <- length(chids)
  cat(paste0("\nCantidad de chids: ", nchids,"\n"))

  dl <- data.frame()
  dlist <- list()
  k<-1
  chid <- 1
  task <- 0
  indant <- 0
  for (i in 1:nchids){ #for (i in 1:nchids
    if (i/1000==floor(i/1000)) cat(paste0("\nCalculando chid ",i))
    if (i/500==floor(i/500)){
      dlist[[k]] <- dl
      k<-k+1
      dl <- data.frame()
    }
    dlaux <- filter(datalong, chid==i)
    indcurr <- dlaux[1, indvar]
    if (choiceXchid$freq[i]==1){
      if (indcurr==indant) task <- task+1 else task <- 1
      dlaux$chid <- chid
      chid <- chid+1
      dlaux[,taskvar] <- task
      dl <- rbind(dl, dlaux)
    } else {
      choices <- which(dlaux$choice==1)
      # print(choices)
      dlaux$choice <- 0
      for (j in choices){
        if (indcurr!=indant & j==choices[1]) task <- 1 else task <-  task+1
        dlaux2 <- dlaux
        dlaux2[j,"choice"] <- 1
        dlaux2$chid <- chid
        chid <- chid+1
        dlaux2[,taskvar] <- task
        dl <- rbind(dl, dlaux2)
      }
    }
    indant <- indcurr
  }

  dlf <- data.frame()
  for (i in 1:(k-1)){
    print(i)
    dlf <- rbind(dlf, dlist[[i]])
  }
  dl <- rbind(dlf, dl)
  # print(dim(dl))
  # print(1)
  cat("\nRecalculando variable chid")
  names(dl)[names(dl)=="choice"] <- choicevar
  names(dl)[names(dl)=="chid"] <- chidvar

  dl <- dcm_add_chid(dl, indvar=indvar, taskvar=taskvar, chidvarname="chid")

  dl

}
