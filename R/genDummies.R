genDummies<- function (data, var, prefix="", width0=NULL) {
  # if (is.null(prefix)) {
  #   prefix <- paste(as.name(alt.var),"_",sep="")
  # }
  categories <- as.character(unique(data[,var])[order(unique(data[,var]))])
  for (cat in categories) {
    if (!is.null(width0)){
      varname <- ifelse(width0>0,prefix0(cat,2),cat)
    } else {
      varname <- cat
    }
    data[,paste(prefix,varname,sep="")] <- as.numeric(data[,var]==cat)
  }
  data
}
