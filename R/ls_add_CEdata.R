ls_add_CEdata <- function(lsd, datachoice, min.task){
  require(dplyr)

  respByRef <- summarise(group_by(datachoice, idReferido), nr = n())
  print(dim(lsd))
  lsd <- merge(x=lsd, y=respByRef, by.x="id", by.y="idReferido", all.x=T)
  print(dim(lsd))
  ind.CEabuse <- respByRef$idReferido[respByRef$nr<=min.task]
  lsd$CEabuse <- as.numeric(lsd$id %in% ind.CEabuse)

  lsd$estado2 <- as.character(lsd$estado)
  lsd$estado2[(lsd$estado=="Completo" | lsd$estado=="LS incompleto")
              & lsd$CEabuse==1] <- "AbusoCE"
  lsd$estado2[lsd$estado2=="Completo"] <- "Validos"
  lsd$estado2 <- factor(lsd$estado2,
                        levels = c("Filtrado", "CE incompleto", "LS incompleto",
                                   "AbusoCE", "Validos"))
  print(table(lsd$estado2))
  lsd
}
