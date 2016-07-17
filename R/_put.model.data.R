# Necesito el datalogit porque cuando descompongo el precio en 2 pendientes en el modelo no tengo el precio real
# R es cuenates veces se va a simular el diseño completo.
# Falta normalizar el diseño
tmp_put.model.data<-function(model, data, datalogit, model.px.var=data$px.var, only.sig=T, design.nsim=1) {
  #browser()
  attach(data,warn.conflicts = F)
  model.mat<-model$model[,which(colnames(model$model)!=data$choice.var)]
  model.mat$chid<-matrix(unlist(strsplit(rownames(model.mat), "\\.")),ncol=2,byrow=T)[,1]
  model.mat$alt<-matrix(unlist(strsplit(rownames(model.mat), "\\.")),ncol=2,byrow=T)[,2]
  renglones.mostrados<- !is.na(model.mat[,1])
  model.mat<-model.mat[!is.na(model.mat[,1]),] # Le quito los renglones con NA por si es Oferta-Precio
  rows<-dim(model.mat)[1]

  vars<-get.info(data=model,type="model",out="vars")
  names.asv<-model.px.var
  n.acv<-length(model.px.var)

  model.mat.base<-model.mat

  dx.table<-demandas

  for (fila in 1:(dim(dx.table)[1])) {

    alter<-dx.table$alt[fila]
    # Selecciono la variable que corresponde con el precio distinto de cero
    px.var.fila<-NULL
    if (n.acv==1) px.var.fila<-model.px.var else {
      for (pxs in model.px.var) {
        if (subset(model.mat.base,alt==alts[alter])[1,pxs]!=0) {px.var.fila<-pxs}
        #           Error en if (subset(model.mat.base, alt == alts[alter])[1, pxs] != 0) { :
        #   argumento tiene longitud cero
        # esto tira si pongo mal las vars.
      }
    }
    #DEBERIA REVISAR SI TENGO NULO MENSAJE DICIENDO QUE LOS NOMBRES DE LSA VARS NO CORRESPONDEN
    #Selecciono los ranglones donde est? el precio que tengo que graficar
    #chids.to.use<-subset(model.mat.base,alt==alts[alter] & model.mat.base[,px.var.fila]==dx.table$px.abs[fila])[,"chid"]
    chids.to.use<-subset(model.mat.base,alt==alter & -model.mat.base[,px.var.fila]==dx.table$px.abs[fila])[,"chid"]
    model.mat<-model.mat.base[model.mat.base$chid %in% chids.to.use,]

    # No es necesario usar muchas simulaciones porque usa todo el dise?o completo con todos los consumidores
    dx.table$dx.model[fila]<-fit.model.rpl(model=model, design=model.mat, design.nsim=1)[alter]
    #browser()
    print(paste(fila/(dim(dx.table)[1])*100,"%"))
  }

  dx.table$logit.dx.model<-log(dx.table$dx.model/(1-dx.table$dx.model))
  dx.table<-dx.table[,c("n","alt","px.abs","px.rel","dx.data","logit.dx.data","dx.model","logit.dx.model")]

  residuos<-dx.table [c("alt","n","px.abs")]
  residuos$ei<-dx.table$dx.model-dx.table$dx.data
  residuos$ei.std<-residuos$ei/sqrt(dx.table$dx.model*(1-dx.table$dx.model)/dx.table$n)

  e.table<-elasticidades
  for (i in 1:nalt) {
    datatemp<-subset(dx.table,alt==i)
    ms2<-max(datatemp$dx.model)
    ms1<-min(datatemp$dx.model)
    p2<-max(datatemp$px.abs)
    p1<-min(datatemp$px.abs)
    e.table$e.model[i]<-((ms2-ms1)/((ms2+ms1)/2))/((p2-p1)/((p2+p1)/2))
  }
  e.table$err.porc<-(e.table$e.model/e.table$e.data-1)*100

  data$data.model<-TRUE
  data$demandas<-dx.table
  data$residuos<-residuos
  data$residuos$sum.ei2<-sum((residuos$ei)^2)
  data$elasticidades<-e.table
  data$formula<-model$formula

  detach(data)

  data
}
