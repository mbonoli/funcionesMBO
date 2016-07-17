ls_setup <- function(lsd, lastpage=NULL, mintime=NULL, maxtime=NULL){

  # ls "estado"
  lsd$estado <- 4
  if (!(is.null(lastpage))){
    lsd$estado[lsd$lastpage<lastpage] <- 3
  } else {
    lsd$estado[lsd$submitdate==""] <- 3
  }
  lsd$estado[lsd$sqlOK!=1] <- 2
  lsd$estado[lsd$filtrado==1] <- 1
  lsd$estado <-
    factor(lsd$estado, levels = 1:4,
           labels = c("Filtrado",  "CE incompleto", "LS incompleto","Completo"))
  print(table(lsd$estado))

  lsd$date <- str_left(lsd$startdate,10)

  lsd$tf_datestamp <- as.POSIXct(strptime(lsd$datestamp, "%Y-%m-%d %H:%M:%S"))
  lsd$tf_startdate <- as.POSIXct(strptime(lsd$startdate, "%Y-%m-%d %H:%M:%S"))
  lsd$tf_submitdate <- as.POSIXct(strptime(lsd$submitdate, "%Y-%m-%d %H:%M:%S"))
  lsd$totaltime <- as.numeric(lsd$tf_datestamp-lsd$tf_startdate)/60
  lsd$date <- str_left(lsd$startdate,10)

  if (!is.null(mintime)){
    lsd$duracion[lsd$totaltime<mintime] <- 1
    lsd$duracion[lsd$totaltime>=mintime & lsd$totaltime<=maxtime] <- 2
    lsd$duracion[lsd$totaltime>maxtime] <- 3
    lsd$duracion <-
      factor(lsd$duracion, levels = 1:3,
             labels = c(paste0("Menos de ",mintime," min"),
                        paste0("Entre ",mintime," y ",maxtime," min"),
                        paste0("Mas de ",maxtime," min")))
    print(table(lsd$duracion))
  }

  lsd
}
