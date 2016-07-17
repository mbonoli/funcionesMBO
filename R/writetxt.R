writetxt <- function(data, filename=deparse(substitute(data)), row.names=F, rowname="row"){
  # writetxt(ppp) Guarda un archivo "ppp.txt" con el dataframe ppp en la carpeta actual

  if (class(data)=="table" & row.names==T){
    tab <- cbind(as.numeric(rownames(data)),data)
    colnames(tab)[1]<- rowname
    write.table(data, paste0(filename,".txt"),
                sep="\t", dec=".", row.names=row.names,
                quote=F)
  } else {
    write.table(data, paste0(filename,".txt"),
                sep="\t", dec=".", row.names=row.names,
                quote=F)
  }

}
