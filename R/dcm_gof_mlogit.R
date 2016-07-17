dcm_gof_mlogit<- function (model.list=list()){
  nm <- length(model.list)
  result <- data.frame(model=1:nm)

  result$model <- names(model.list)
  for (i in 1:nm){
    result$logLik[i] <- round(model.list[[i]]$logLik,1)
    result$AIC[i] <- round(AIC(model.list[[i]]),1)
    result$BIC[i] <- round(sbc.logit(model.list[[i]]),1)
    result$R2_L0[i] <- round(mlogit.R2_L0(model.list[[i]]),3)
  }
  result$rank_Lik <- nm - rank(result$logLik) + 1
  result$rank_AIC <- rank(result$AIC)
  result$rank_BIC <- rank(result$BIC)
  result$rank_R2_L0 <- nm-rank(result$R2_L0)+1

  for (i in 1:nm){
    result$formula[i] <- paste(as.character(model.list[[i]]$formula)[2],"~",as.character(model.list[[i]]$formula)[3],sep=" ")
  }
  model.list <- model.list[order(result$rank_BIC)]
  result <- result[order(result$rank_BIC),]
  result
}
