tmp_get.fitted.rpl<-function (model, data, acv=NA, alt.view=NA, only.sig=F,  R=5000, betas.post=NA) {
  #browser()
  #coef<-summary(model)$CoefTable[,1]    # Esto es para modelos construidos con mlogit .8
  coef<-coefficients(model)              # Esto es para modelos construidos con mlogit .4
  nalter<-data$nalt
  names.vars<-get.info(model,type="model",out="vars")
  if (data$is.px.var==T) {
    if (is.na(acv)[1]) {
      print ("El modelo es de precios variables y debe ingresar un vector de precios (acv)")
      break}
    else {
      names.acv<-rownames(acv)
    }
  }
  if (data$is.of.var==T) {
    if (is.na(alt.view)[1]) stop ("El modelo es de oferta variables y debe ingresar un vector alt.view")
    else if (length(alt.view)!=nalter) stop ("alt.view debe contener 24 valores")
    else names.acv<-""
  }
  names.asv<-names.vars[which(names.vars!=names.acv)]
  n.var.asv<-length(names.asv)

  # Matriz de transformaci?n de asv
  #mt.asv<-model$model[1:nalter,which(colnames(model$model)!="choice" & colnames(model$model)!=names.acv & colnames(model$model)!="(weights)")]
  mt.asv<-model$model[1:nalter,which(colnames(model$model)!="choice" & !(colnames(model$model) %in% names.acv) & colnames(model$model)!="(weights)")]
  #mt.asv[refer]<-0

  if (is.na(betas.post)[1]==T) {
    # Guardo las medias y desv?os tomados del modelo
    medias.asv<-coef[names.asv]
    sd.asv<-abs(coef[paste("sd.",names.asv,sep="")])

    if (only.sig) {
      no.sig <- summary(model)$CoefTable[,4]>0.10
      medias.asv[no.sig[names.asv]]<-0
      sd.asv[no.sig[paste("sd.",names.asv,sep="")]]<-0
    }

    # Busco la referencia que es la que no aparece en el modelo pero si en mt
    # id.ref<-which(is.na(medias.asv))
    # Guardo el nombre y la media de la variable de referencia
    # names(medias.asv)[id.ref]<-names.asv[id.ref]
    #medias.asv[id.ref]<-0
    #names(sd.asv)<-names(medias.asv)
    #sd.asv[id.ref]<-0

    # Guardo las medias de las alternativa com?n variables
    names.acv<-rownames(acv)
    n.acv<-length (names.acv)
    medias.acv<-coef[names.acv]
    sd.acv<-abs(coef[paste("sd.",names.acv,sep="")])

    betas.simulados.asv<-matrix(nrow=R,ncol=n.var.asv)
    colnames(betas.simulados.asv)<-	names.asv
    for (i in 1:n.var.asv) {betas.simulados.asv[,i]<-rnorm(R,mean=medias.asv[i],sd=sd.asv[i])}
    ln.util.asv <- betas.simulados.asv %*% t(mt.asv)

    colnames(ln.util.asv)<-genBetasNames(nalter)

    if (data$is.px.var) {
      betas.simulados.acv<-matrix(nrow=R,ncol=n.acv)
      colnames(betas.simulados.acv)<-	names.acv
      for (i in 1:n.acv) {betas.simulados.acv[,i]<-rnorm(R,mean=medias.acv[i],sd=sd.acv[i])}
      ln.util.acv<-betas.simulados.acv %*% acv
    } else ln.util.acv<- 0


  }
  else {
    ncons<- dim(betas.post)[1]
    betas.simulados.asv<-betas.post[,which(colnames(betas.post)!="cons" & colnames(betas.post)!=names.acv )]
    colnames(betas.simulados.asv)<-	names.asv
    ln.util.asv <- betas.simulados.asv %*% t(mt.asv)
    colnames(ln.util.asv)<-genBetasNames(nalter)
    if (names.acv!="") {
      betas.simulados.acv<-betas.post[,names.acv]
      ln.util.acv<-betas.simulados.acv %*% acv
    } else ln.util.acv<-0
  }

  util<-exp(ln.util.asv+ln.util.acv)
  if (data$is.of.var) util[,which(alt.view==0)] <- 0          #Esto es para que solo considere lo visto
  SumaUtil<-apply(util,1,sum)

  pi<-util
  for (i in 1:nalter) {pi[,i]<-util[,i]/SumaUtil}

  ms<-apply(pi,2,mean)

  ms
}
