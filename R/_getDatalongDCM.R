# Esta funcion es la misma que getDataDCM pero en vez de trear solo los resultados trae tambi?n el Dise?o.
# Es funcion por lo tanto no trae la alternativa 999 para saber cuales son los viajes que vió el consumidor
tmp_getDatalongDCM<-function (idencuesta, id.encuestado.minimo=1,id.encuestado.maximo=1000000, bits=64) {
  browser()
  require(RODBC)
  require(sqldf)
  print ("Tarda mucho por el cruce con la tabla de encuestados y el ordenamiento")

  if (bits==64) channel <- odbcConnect("Surveyweb DCM 64") else channel <- odbcConnect("Surveyweb DCM 32")
  print (paste("IdEncuesta: ",idencuesta,sep=""))
  enc.name<-get.NameDCM(idencuesta,bits=bits)[,1]
  print (paste("Encuesta: ",enc.name,sep=""))
  print (paste("Viajes: ",get.nviajesDCM(idencuesta,bits=bits),sep=""))
  print (paste("Subviajes: ",get.nsubviajesDCM(idencuesta,bits=bits),sep=""))
  nalt<-get.nalternativasDCM(idencuesta,bits=bits)
  print (paste("Alternativas: ",nalt,sep=""))

  nsv<-sqlQuery(channel, paste("Select distinct IdSubviaje from respuestas where idencuesta = ",idencuesta," order by IdSubviaje",sep=""))[,1]

  for (i in nsv) {
    d<-sqlQuery(channel,paste("select * from disenos where idencuesta=",idencuesta,sep=""))
    #d<-sqlQuery(channel,paste("select * from disenos where idencuesta=",idencuesta," and idalternativa<=",nalt,sep=""))
    e<-sqlQuery(channel,paste("select * from encuestados where idencuesta=",idencuesta,sep=""))
    if (i==1) {
      r<-sqlQuery(channel,paste("select * from respuestas where idencuesta=",idencuesta," and idsubviaje=1",sep=""))
      datos<-sqldf(paste("
                         select distinct d.IdEncuesta, e.IdReferido cons, e.Bloque, d.IdViaje, 1 as IdSubviaje, d.IdAlternativa, cod, PrecioBase precio, Cantidad, Primero, Time
                         from d
                         left join e on d.bloque=e.bloque
                         left join r on r.idviaje=d.idviaje and r.idalternativa=d.idalternativa and r.idencuestado=e.idencuestado
                         where e.IdReferido>=",id.encuestado.minimo," and e.IdReferido<=",id.encuestado.maximo,sep=""))
    }
    else {
      r<-sqlQuery(channel,paste("select * from respuestas where idencuesta=",idencuesta," and idsubviaje=",i,sep=""))
      datos<-rbind(datos, sqldf(paste("
                                      select distinct d.IdEncuesta, e.IdReferido cons, e.Bloque, d.IdViaje, ",i," as IdSubviaje, d.IdAlternativa, cod, PrecioBase precio, Cantidad, Primero, Time
                                      from d
                                      left join e on d.bloque=e.bloque
                                      left join r on r.idviaje=d.idviaje and r.idalternativa=d.idalternativa and r.idencuestado=e.idencuestado
                                      where e.IdReferido>=",id.encuestado.minimo," and e.IdReferido<=",id.encuestado.maximo,sep="")))
    }
    print (paste("fin viaje", i,sep=""))}
  datos<-sqldf("select * from datos order by cons, idviaje,idsubviaje,idalternativa")
  names(datos)[which(names(datos)=="cod")]<-"alter" # Esto es porque no se puede usar la palabra alter en el sql
  odbcCloseAll()
  datos$Cantidad[which(is.na(datos$Cantidad))]<-0
  datos$Primero[which(is.na(datos$Primero))]<-0
  print (paste("Primer cons: ",min(datos$cons),sep=""))
  print (paste("?ltimo cons: ",max(datos$cons),sep=""))
  print (paste("Cantidad de encuestados: ",length(unique(datos$cons)),sep=""))
  datos
    }
