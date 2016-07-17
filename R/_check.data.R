_check.data <- function (data) {

  # Choice Share == 0
  choice0<-sum(data$choice.share==0)
  if (choice0==0) {
    cat("\nAlaternativas sin elecci?n: No hay   (OK)")
  }
  else {
    alts<-names(data$choice.share)[which(data$choice.share==0)]
    cat(paste("\nAlaternativas sin elecci?n:",alts,"(EL MODELO NO ES ESTIMABLE)"))
  }

  # Plot Choice Share
  barplot(data$choice.share, main="CHOICE SHARE", xlab="Alternativa",names.arg=names(data$choice.share),cex.names=0.8)

  # Presencia vs Presencia
  design<-data$design$matrix
  mat1<-t(design)%*%design
  max<-max(mat1)
  mat2<-mat1-diag(mat1)*diag(nrow=25)+max*diag(nrow=25)
  mf<-min(mat2)
  if (mf==0) {
    cat("\nPresencias * Presencia de las dem?s: EXISTEN ALTERNATIVAS QUE NO SE MUESTRA CON EL RESTO   (EL MODELO NO ES ESTIMABLE)")
    cat("\n")
    print(mat1)
  }
  else {
    cat("\nPresencias * Presencia de las dem?s: Todas se muestran con todas   (OK)")
  }

  # Presencia vs Ausencia
  design<-data$design$matrix
  mat1<-t(design)%*%(1-design)
  max<-max(mat1)
  mat2<-mat1-diag(mat1)*diag(nrow=25)+max*diag(nrow=25)
  mf<-min(mat2)
  if (mf==0) {
    cat("\nPresencias * Ausencia de las dem?s: EXISTEN ALTERNATIVAS QUE NO SE MUESTRA CON EL RESTO   (EL MODELO NO ES ESTIMABLE)")
    cat("\n")
    print(mat1)
  }
  else {
    cat("\nPresencias * Ausencia de las dem?s: Todas se muestran con todas   (OK)")
  }

  # choice con alter
  mat1<-data$freq.r.con
  max<-max(mat1)
  mat2<-mat1-diag(mat1)*diag(nrow=25)+max*diag(nrow=25)
  mf<-min(mat2)
  if (mf==0) {
    cat("\nChoice * Presencia de las dem?s: EXISTEN ALTERNATIVAS QUE NO SE ELIGEN CON EL RESTO   (EL MODELO NO ES ESTIMABLE)")
    cat("\n")
    print(mat1)
  }
  else {
    cat("\nChoice * Presencia de las dem?s: Todas se eligen con todas   (OK)")
  }

  # choice sin alter
  mat1<-data$freq.r.sin
  max<-max(mat1)
  mat2<-mat1-diag(mat1)*diag(nrow=25)+max*diag(nrow=25)
  mf<-min(mat2)
  if (mf==0) {
    cat("\nChoice * Ausencia de las dem?s: EXISTEN ALTERNATIVAS QUE NO SE ELIGEN CON EL RESTO   (EL MODELO NO ES ESTIMABLE)")
    cat("\n")
    print(mat1)
  }
  else {
    cat("\nChoice * Ausencia de las dem?s: Todas se eligen con todas   (OK)")
  }
}
