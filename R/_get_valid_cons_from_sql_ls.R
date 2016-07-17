tmp_get_valid_cons_from_sql_ls<-function (sid, id.encuestado.minimo=1,id.encuestado.maximo=1000000, bits=64) {
  require(RODBC)
  if (bits==64) channel <-odbcConnect("LS 64") else channel <- odbcConnect("LS 32")
  vc<-sqlQuery(channel, paste(
    "SELECT id FROM mbonoli_ls2.lime_survey_",sid,
    " where submitdate is not NULL and id>=",id.encuestado.minimo," and id<=",id.encuestado.maximo,sep=""))
  vc
}
