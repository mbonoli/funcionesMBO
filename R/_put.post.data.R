tmp_put.post.data<-function(model, data, betas.post, model.px.var=data$px.var, only.sig=T, R=8000) {
  browser()

  attach(data,warn.conflicts = F)
  model.mat<-model$model[,which(colnames(model$model)!=data$choice.var)]
  model.mat$chid<-matrix(unlist(strsplit(rownames(model.mat), "\\.")),ncol=2,byrow=T)[,1]
  model.mat$alt<-matrix(unlist(strsplit(rownames(model.mat), "\\.")),ncol=2,byrow=T)[,2]
  model.mat$cons<-attr(model$model,"index")[,"id"]
  renglones.mostrados<- !is.na(model.mat[,1])
  model.mat<-model.mat[!is.na(model.mat[,1]),] # Le quito los renglones con NA por si es Oferta-Precio
  rows<-dim(model.mat)[1]

  vars<-get.info(data=model,type="model",out="vars")
  names.asv<-px.var
  n.acv<-length(model.px.var)

  model.mat.base<-model.mat

  dx.table<-demandas

  pij<-fit.post.rpl(model=model, design=model.mat, betas.post=data$betas.indiv)

  for (fila in 1:(dim(dx.table)[1])) {

    #model.mat<-model.mat.base
    alter<-dx.table$alt[fila]
    # Selecciono la variable que corresponde con el precio distinto de cero
    if (n.acv==1) px.var.fila<-model.px.var else {
      for (pxs in model.px.var) {
        if (subset(model.mat.base,alt==alts[alter])[1,pxs]!=0) {px.var.fila<-pxs}
      }
    }
    precio<-dx.table$px.abs[fila]
    chids.a.considerar<-model.mat[model.mat$alt==alts[alter] & model.mat[,px.var.fila]==precio,"chid"]

    #for (i in px.slope) {model.mat[which(model.mat$alt==data$alts[alter] & model.mat[i]!=0),i]<- precio.reemplazar }
    dx.table$dx.post[fila]<-mean(pij[pij$chid %in% chids.a.considerar & pij$alt==alts[alter],"pij"])
    #browser()
    print(paste(fila/(dim(dx.table)[1])*100,"%"))
  }

  dx.table$logit.dx.post<-log(dx.table$dx.post/(1-dx.table$dx.post))
  dx.table<-dx.table[,c("n","alt","px.abs","px.rel","dx.data","logit.dx.data","dx.model","logit.dx.model","dx.post","logit.dx.post")]

  residuos<-dx.table [c("alt","n","px.abs")]
  residuos$ei.post<-dx.table$dx.post-dx.table$dx.data
  residuos$ei.post.std<-residuos$ei.post/sqrt(dx.table$dx.model*(1-dx.table$dx.model)/dx.table$n)

  e.table<-data$elasticidades
  for (i in 1:data$nalt) {
    datatemp<-subset(dx.table,alt==i)
    ms2<-max(dx.table$dx.post)
    ms1<-min(dx.table$dx.post)
    p2<-max(dx.table$px.abs)
    p1<-min(dx.table$px.abs)
    e.table$e.post[i]<-((ms2-ms1)/((ms2+ms1)/2))/((p2-p1)/((p2+p1)/2))
  }
  e.table$err.porc.post<-(e.table$e.post/e.table$e.data-1)*100

  data$data.post<-TRUE
  data$demandas<-dx.table
  data$residuos<-residuos
  data$residuos$sum.ei2.post<-sum((residuos$ei.post)^2)
  data$elasticidades<-e.table
  data$formula<-model$formula

  data
}
