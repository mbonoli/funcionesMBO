#De momento solo funciona BB
dcm_alternative_delete_BB <- function(datalong,
                                      chid.var="chid", 
                                      alt.var="alt", altid.var="altid",
                                      choice.var="choice",
                                      task.var="task", subtask.var="subtask", ind.var="ind"){
  require(dplyr)
  require(funcionesMBO)
  dl <- datalong
  names(dl)[names(dl)==chid.var]<-"chidv"
  names(dl)[names(dl)==alt.var]<-"altv"
  names(dl)[names(dl)==altid.var]<-"altidv"
  names(dl)[names(dl)==choice.var]<-"choicev"
  names(dl)[names(dl)==task.var]<-"tv"
  names(dl)[names(dl)==subtask.var]<-"stv"
  names(dl)[names(dl)==ind.var]<-"indv"

  dl2 <- filter(dl, stv==1 & choicev==1)
  dl2 <- select(dl2, indv, tv, stv, delalt=altv)
  dl2$stv <- 2
  dl <- merge(x=dl, y=dl2, by=c("indv", "tv", "stv"),all.x=T)

  nr.old <- dim(dl)[1]
  dl <- filter(dl, !(altv==delalt & stv==2 & choicev!=1)) # lo del choice es porque puede ser una tarea amalgamada
  dl <- arrange(dl, indv, tv, stv, altv)
  dl <- dcm_add_alt.id(dl, chid.var = "chidv", 
                       alt.var="altv", altid.var = "altidv")
  nr.new <- dim(dl)[1]
  cat(paste0("\nSe eliminaron alternativas en ", nr.old-nr.new," sub-tareas."))

  dl <- dl[,!(names(dl) %in% c("delalt")) ]
  names(dl)[names(dl)=="chidv"]<-chid.var
  names(dl)[names(dl)=="altv"]<-alt.var
  names(dl)[names(dl)=="altidv"]<-altid.var
  names(dl)[names(dl)=="choicev"]<-choice.var
  names(dl)[names(dl)=="tv"]<-task.var
  names(dl)[names(dl)=="stv"]<-subtask.var
  names(dl)[names(dl)=="indv"]<-ind.var

  dl
}
