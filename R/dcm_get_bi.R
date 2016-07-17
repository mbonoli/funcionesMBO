dcm_get_bi<-function (model,
                      datalong,
                      choice.var="choice",
                      cons.var="cons",
                      chid.var="chid",
                      alt.var="IdAlternativa",
                      R.bi=50) {

  #Guardo los nombres de los coeficientes
  coefficients<-names(model$coefficients)
  ncoef<-length(coefficients)
  alt.levels<-unique(datalong[,alt.var])
  #Guardo los nombres de las variables
  var.names<-coefficients[substr(coefficients,1,3)!="sd."]
  nvar<-length(var.names)
  #Guardo los nombres de las variables random
  random.names<-substr(coefficients[substr(coefficients,1,3)=="sd."],4,100)
  nrandom<-length(random.names)
  #Guardo los nombres de las variables fixed
  fixed.names<-var.names[coefficients[substr(coefficients,1,3)!="sd."] != random.names]
  nfixed<-length(fixed.names)

  medias<-c(model$coefficients[random.names],model$coefficients[fixed.names])
  desvios<-c(abs(model$coefficients[paste("sd.",random.names,sep="")]),rep(0,nfixed))
  names(desvios)<-names(medias)

  consumidores<-unique(datalong[,cons.var])
  ncons<-length(consumidores)
  print (paste("Número de consumidores ",ncons,sep=""))

  tmp <- unique(attr(model$model,"index")["alt"])
  nalt <- length(levels(tmp$alt))

  estim_B<-matrix(ncol=1+nvar,nrow=ncons)
  colnames(estim_B)<-c("cons",var.names)

  cons<-0
  #browser()
  for (consumidor in consumidores) {
    #       consumidor<-consumidores[2]
    #Datos del cosumidor
    data<-datalong[,c(cons.var,alt.var,chid.var,var.names,choice.var)]
    #Me quedo solo con el consumidor
    data<- data[which(data[,cons.var]==consumidor),]
    #browser()
    prodPjDensidad<-NULL
    prodPjDensidadBi<-NULL

    for (i in 1:R.bi) {
      #Aleatorios Normal estandar
      rn<-rnorm(nvar,0,1)
      SimBetas<- c()
      for (i in 1:nvar) {
        if (model$rpar[[names(medias)[i]]]$dist=="n") {
          SimBetas <- c(SimBetas, rnorm(1,0,1)*desvios[i]+medias[i])
        }
        else if (model$rpar[[names(medias)[i]]]$dist=="ln") {
          SimBetas <- c(SimBetas, exp(rnorm(1,0,1)*desvios[i]+medias[i]))
        }
      }
      # Betas Aleatorios
      # Simulo todos los betas, aunque no los use este individuo
      SimBetas<-rn*desvios+medias
      # Calculo los niveles que no usa este individuo
      dropped.levels <- as.character(alt.levels[!(alt.levels %in% unique(data[,alt.var]))])
      # Saco las constantes que el individuo no usa
      if (length(dropped.levels)>0) {
        var.names.indiv <- var.names[var.names!=dropped.levels]
      } else {var.names.indiv <- var.names}
      #Numeradores de las probabilidades.
      # 26.05.2011 Agrego [var.names] para que tengan el mismo orden al hacer el producto
      numProb<-exp(as.matrix(data[,var.names.indiv])%*%as.matrix(SimBetas[var.names.indiv]))
      #denProb<-rep(tapply(numProb,data$chid,sum),each= table(datalong$chid))
      denProb<-rep(tapply(numProb,data[,chid.var],sum),times= table(data[,chid.var]))
      #browser()
      prob<-numProb/denProb
      prodPj<-prod(prob[which(data$choice==TRUE)])

      prodPjDensidad<-rbind(prodPjDensidad,prodPj)
      prodPjDensidadBi<-rbind(prodPjDensidadBi,prodPj*SimBetas[var.names.indiv])
    }

    sumaNum<-colSums(prodPjDensidadBi)
    sumaDen<-sum(prodPjDensidad)

    estim_B[which(consumidores==consumidor),c("cons",names(sumaNum))]<-c(consumidor,sumaNum/sumaDen)
    cons<-cons+1
    print (paste("cons ",cons,"/",ncons,": ",consumidor,sep=""))
  }
  estim_B
}
