#Devuleve el N? viajes de una encuesta de acuerdo a lo cargado en la BD
tmp_get.nviajesDCM<-function(idencuesta, bits=64) {
  require(RODBC)
  if (bits==64) channel <- odbcConnect("Surveyweb DCM 64") else channel <- odbcConnect("Surveyweb DCM 32")
  result<-as.numeric(sqlQuery(channel, paste("Select NumeroViajes from encuestas where idencuesta = ",idencuesta,sep="")))
  close(channel)
  result
}
