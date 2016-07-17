dcm_bi_get <- function(model, par="mu"){
  if (class(model)!="gmnl") stop("El modelo debe ser gmnl")
  if (!(par %in% c("mu", "sd"))) stop("El parámetro par solo puede tomar los valores 'mu' o 'sd'")
  bi_data <- model$bi
  nind <- dim(bi_data)[1]
  R <- dim(bi_data)[2]
  npar <- dim(bi_data)[3]
  cat(paste0("\nNumero de individuos: ",nind))
  cat(paste0("\nNumero de parámetros: ",npar))
  cat(paste0("\nNumero de simulaciones R: ",R,"\n"))
  if (par=="mu") result <- apply(bi_data,c(1,3),mean)
  else result <- apply(bi_data,c(1,3),sd)
  result <- as.data.frame(result)
  cn <- colnames(result)
  result$ind <- 1:nind
  result[,c("ind", cn)]
}
