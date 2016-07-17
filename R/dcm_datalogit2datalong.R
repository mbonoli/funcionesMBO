dcm_datalogit2datalong<- function (datalogit) {
  datalong<-as.data.frame(datalogit)
  row.names(datalong)<-NULL
  datalong
}
