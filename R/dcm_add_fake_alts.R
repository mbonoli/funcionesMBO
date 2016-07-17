dcm_add_fake_alts <- function(datalong, alt.var="alt",
                              chid.var="chid", ind.var="ind"){
  alt.range <- unique(datalong[,alt.var])
  chids <- unique(datalong[,chid.var])
  dl <- expand.grid(altid=alt.range, chid=chids)[,c(2,1)]
  dl <- distinct(merge(x=dl, y=datalong[,c(chid.var, ind.var)], by=c(chid.var)))
  dl <- merge(x=dl, y=datalong, by=c(alt.var, chid.var, ind.var), all.x=T)
  dl <- dl[order(dl[,ind.var],dl[,chid.var],dl[,alt.var]),]
  dl
}



