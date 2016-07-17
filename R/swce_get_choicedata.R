swce_get_choicedata <- function (ide, include.999=T) {
  require(RMySQL)
  mydb = dbConnect(MySQL(),
                   user='mbonoli_mbonoli',
                   password='burlhorse31', dbname='mbonoli_dcm_v5',
                   host='192.254.235.235')
  # dbListTables(mydb)
  query <- paste0("select e.IdReferido idReferido, e.Bloque bloque, r.* from respuestas r
                  left join encuestados e on r.idencuestado=e.idencuestado
                  where r.idencuesta=",ide,
                  ifelse(include.999, "", " and idAlternativa<>999"))
  rs = dbSendQuery(mydb, query)
  data = fetch(rs, n=-1)
  data
}

