# De momento solo funciona para modelos de mkt con o sin px
dcm_alternative_fusion <- function(datalong, source.alt, dest.alt,
                                   chid.var="chid", alt.var="alt", altid.var="altid",
                                   choice.var="choice",
                                   task.var="task", subtask.var="subtask", ind.var="ind", px.var=NULL,
                                   drop.fus.subtask.choiced=T,
                                   descr.var, descr.val){
  # fus.subtask.choiced.delete=F no está implementado
  require(dplyr)
  require(funcionesMBO)
  dl <- datalong
  names(dl)[names(dl)==chid.var]<-"chidv"
  names(dl)[names(dl)==alt.var]<-"altv"
  names(dl)[names(dl)==altid.var]<-"altidv"
  names(dl)[names(dl)==task.var]<-"tv"
  names(dl)[names(dl)==subtask.var]<-"stv"
  names(dl)[names(dl)==choice.var]<-"choicev"
  names(dl)[names(dl)==ind.var]<-"indv"

  altparaquitar <- source.alt[!(source.alt %in% dest.alt)]
  if (!is.null(px.var)){
    # browser()
    names(dl)[names(dl)==px.var]<-"pxv"
    dl1 <- filter(dl, altv %in% source.alt)
    dl1 <- summarise(group_by(dl1, chidv), sumchoice=sum(choicev), meanpx=mean(pxv))
    dl <- merge(x=dl, y=dl1, by="chidv", all.x=T)
    dl <- filter(dl, !(altv %in% altparaquitar))
    # No quitar el !is.na(dl$sumchoice) porque puede ser una subtarea que esté oculta la alternativa buscada
    dl[dl$altv == dest.alt & !is.na(dl$sumchoice), "choicev"] <-
      ifelse(dl[dl$altv == dest.alt & !is.na(dl$sumchoice), "choicev"]==1,1,dl[dl$altv == dest.alt & !is.na(dl$sumchoice), "sumchoice"])
    dl[dl$altv == dest.alt & !is.na(dl$sumchoice), "pxv"] <- dl[dl$altv == dest.alt & !is.na(dl$sumchoice), "meanpx"]
    names(dl)[names(dl)=="pxv"]<-px.var
  } else {
    dl1 <- filter(dl, altv %in% source.alt)
    dl1 <- summarise(group_by(dl1, chidv), sumchoice=sum(choicev))
    dl <- merge(x=dl, y=dl1, by="chidv")
    dl <- filter(dl, !(altv %in% altparaquitar))
    dl[dl$altv == dest.alt, px.var] <- dl[dl$altv == dest.alt, "meanpx"]
  }
  if (drop.fus.subtask.choiced){
    choiceXchid <- summarize(group_by(dl, chid=chidv), freq=sum(choicev))
    del.chid <- filter(choiceXchid, freq==0)$chid
    cat("\ndrop.fus.subtask.choiced = TRUE")
    cat(paste0("\nSe eliminaron ", length(del.chid), " chids sin choice."))
    dl <- filter(dl, !(chidv %in% del.chid))
  }
  dl <- dcm_add_alt.id(dl, chid.var = "chidv", 
                       alt.var="altv", altid.var = "altidv")
  dl <- dcm_add_chid(dl,indvar = "indv",taskvar = "tv", subtaskvar = "stv", chidvarname = "chidv")
  
  dl[dl$altv==dest.alt, descr.var] <- descr.val
  
  names(dl)[names(dl)=="chidv"]<-chid.var
  names(dl)[names(dl)=="altv"]<-alt.var
  names(dl)[names(dl)=="altidv"]<- altid.var
  names(dl)[names(dl)=="tv"]<-task.var
  names(dl)[names(dl)=="stv"]<-subtask.var
  names(dl)[names(dl)=="choicev"]<-choice.var
  names(dl)[names(dl)=="indv"]<-ind.var
  dl <- dl[,!(names(dl) %in% c("sumchoice","meanpx")) ]


  dl
}
