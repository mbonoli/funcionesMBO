swce_get_design <- function (ide) {
  require(RMySQL)
  mydb = dbConnect(MySQL(),
                   user='mbonoli_mbonoli',
                   password='burlhorse31', dbname='mbonoli_dcm_v5',
                   host='192.254.235.235')
  # dbListTables(mydb)
  query <- paste0("select * from disenos
                  where idencuesta=",ide)
  rs = dbSendQuery(mydb, query)
  data = fetch(rs, n=-1)
  data
}
