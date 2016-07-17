test_colin <- function(formula, data, matcor=F){
  require(car)
  y <- all.vars(formula)[1]
  xs <- all.vars(formula)[-1]
  model <- lm(formula = formula, data = data)
  cat(paste0("DET = ", det(cor(data[,xs])),"\n"))
  cat("\n\nVIFs > 10\n")
  if (sum(vif(model)>10)>0) print(vif(model)[vif(model)>10]) else cat("No hay")
  cat("\n\nVIFs > 5\n")
  if (sum(vif(model)>5 & vif(model)<=10)>0) print(vif(model)[vif(model)>5 & vif(model)<=10]) else cat("No hay")
  cat("\n\nOtros VIFs\n")
  if (sum(vif(model)<=5)>0) print(vif(model)[vif(model)<=5]) else print("No hay")
  if (matcor) cor(data[,xs])
}
