dcm_add_alt.id <- function(datalong, chid.var="chid", alt.var ="alt", altid.var="altid"){
  datalong <- datalong[order(datalong[,chid.var],datalong[,alt.var]),]
  naltbychid <- as.numeric(table(datalong[, chid.var]))
  datalong[, altid.var] <- unlist(lapply(naltbychid, seq))
  datalong
}


