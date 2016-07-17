#Devuelve el título de una encuesta de acuerdo a lo cargado en la BD
tmp_get.NameDCM<-function(idencuesta, bits=64) {
  require(RODBC)
  if (bits==64) channel <- odbcConnect("Surveyweb DCM 64") else channel <- odbcConnect("Surveyweb DCM 32")
  result<-sqlQuery(channel, paste("Select Nombre from encuestas where idencuesta = ",idencuesta,sep=""))
  close(channel)
  result
}
