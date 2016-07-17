dcm_gof_R2_L0 <- function (model){

  # genero una tabla de frecuencia con la cantidad de registros para cada chid
  altByChid <- table(attr(model$model,"index")[["chid"]])
  #   head(altByChid,20)

  # Calculo la probabilidad para cada chid suponiendo máxima incertidumbre
  probByChid <- 1/altByChid
  #   head(probByChid,20)

  # Calcula el logaritmo verosimilitud como la suma de los logaritmos de pi
  L0 <- sum(log(probByChid))

  1-as.numeric(model$logLik)/L0

}
