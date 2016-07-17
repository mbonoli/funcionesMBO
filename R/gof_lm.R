gof_lm <- function (model, log=FALSE){
  # Imprime en pantalla el MAPE y WAPE para un modelo lm
  print(paste0("R2          = ", round(100*summary(model)$r.squared,1), " %"))
  if (log==TRUE){
    y.var <- exp(model[["model"]][,1])
    y.pred <- exp(predict(model))
  } else{
    y.var <- model[["model"]][,1]
    y.pred <- predict(model)
  }
  print(paste0("MAPE mean   = ", round(100*mean(abs(y.var-y.pred)/y.var),1), " %"))
  print(paste0("MAPE median = ", round(100*median(abs(y.var-y.pred)/y.var),1), " %"))
  print(paste0("MAPE +40%   = ", round(100*sum(abs(y.var-y.pred)/y.var>.4)/length(y.var),1), " %"))
  print(paste0("WAPE        = ", round(100*sum(abs(y.var-y.pred))/sum(y.var),1), " %"))
}
