# Esta función genera un datalong para deseños de oferta variable
dcm_wide2long <- function(datawide, nalt,
                          indvar, blockvar, taskvar,
                          altvarLong="alt", chidvarLong = "chid",
                          viewPrefix=NA, viewvarLong="view", onlyviewdata=T,
                          pricePrefix=NA, pricevarLong="precio", priceType=c("price", "mult", "var"), pxBase=NA,
                          choicePrefix=NA, choicevarLong="choice",
                          isBW=F, Bvar, Wvar,
                          genDummies=T,
                          is.multiplechoice=T, # Esto se debería dar cuenta solo
                          sep=""){

  require(dplyr)
  require(funcionesMBO)

  # Showvars
  if (!is.na(blockvar)){
    showvars <- c(indvar, blockvar, taskvar, altvarLong, chidvarLong)
  } else {
    showvars <- c(indvar, taskvar, altvarLong, chidvarLong)
  }

  # Variables IDs
  if (!is.na(blockvar)) idvars <- c(indvar, blockvar, taskvar)
  else idvars <- c(indvar, taskvar)

  # Variables Varing
  varingvars <- c()
  var.names <- c()
  if (!is.na(viewPrefix)) {
    varingvars <- c(varingvars, paste0(viewPrefix,rep(prefix0(1:nalt,2))))
    var.names <- c(var.names, viewvarLong)
  }
  if (!is.na(pricePrefix)) {
    varingvars <- c(varingvars, paste0(pricePrefix,rep(prefix0(1:nalt,2))))
    var.names <- c(var.names, pricevarLong)
  }
  if (!is.na(choicePrefix)) {
    varingvars <- c(varingvars, paste0(choicePrefix,rep(prefix0(1:nalt,2))))
    var.names <- c(var.names, choicevarLong)
  }

  # Reshape
  cat("\nGenerando el Datalong")
  datalong <- reshape(datawide,
                      varying= varingvars,
                      idvar = chidvarLong,
                      # v.names = var.names, No poner porque cambiar el orden de las variables
                      direction="long",
                      sep=sep,
                      timevar=altvarLong)
  names(datalong)[names(datalong)==viewPrefix] <- viewvarLong
  names(datalong)[names(datalong)==choicePrefix] <- choicevarLong

  # Ajustes de Precios
  cat("\nAgregando variables de precios")
  if (!is.na(pricePrefix)) {
    if(is.na(priceType)) stop("Se requiere un valor para el parámetro 'priceType'")
    if(!(priceType %in% c("price", "mult", "var"))) stop("Se requiere un valor para el parámetro 'priceType' price/mult/var")
    if (priceType=="price"){
      names(datalong)[names(datalong)==pricePrefix] <- pricevarLong
    } else {
      if (is.na(pxBase)[1]) stop("Se requiere un vector de precios base: 'pxBase'")
      if (length(pxBase)!=nalt) stop("Se requiere que la longitud del vector de precios base 'pxBase' coincida con 'nalt'")
      names(datalong)[names(datalong)==pricePrefix] <- paste0(priceType,"_",pricevarLong)
      px <- data.frame(alt=1:nalt, pxBase=pxBase)
      datalong <- merge(x=datalong, y=px, by.x=altvarLong, by.y="alt", all.x=T)
      if (priceType=="mult"){
        datalong[, pricevarLong] <- datalong$pxBase * datalong[, paste0(priceType,"_",pricevarLong)]
        showvars <- c(showvars, paste0(priceType,"_",pricevarLong), "pxBase")
      } else {
        # es var
        datalong[, pricevarLong] <- datalong$pxBase * (1+datalong[, paste0(priceType,"_",pricevarLong)])
        showvars <- c(showvars, paste0(priceType,"_",pricevarLong), "pxBase")
      }
    }
    showvars <- c(showvars, pricevarLong)
    # print (showvars)
  }

  # Elimino alternativas no vistas
  cat("\nEliminando renglones no vistos")
  if (onlyviewdata) datalong <- filter(datalong, view==1)

  if (isBW) {
    cat("\nGenerando las tareas B-W")
    datalong$orig.task <- datalong[,taskvar]
    dat_b <- dat_w <- datalong
    dat_b$choice <- dat_b[,Bvar]==dat_b[,altvarLong]
    dat_b[,taskvar] <- dat_b[,taskvar]*2-1
    dat_b$type <- "b"
    dat_w$choice <- dat_w[,Wvar]==dat_w[,altvarLong]
    dat_w[,taskvar] <- dat_w[,taskvar]*2
    dat_w$type <- "w"
    datalong <- rbind(dat_b, dat_w)
    showvars <- c(showvars, "type")
    datalong <- dcm_add_chid(datalong, indvar = indvar, taskvar = taskvar, chidvarname=chidvarLong)
  }

  showvars <- c(showvars, choicevarLong)
  if(genDummies){
    cat("\nGenerando Dummies de Alternativa")
    datalong <- genDummies(datalong, alt.var="alt",prefix="B", width0=2)
    dummiesvars <- paste0("B",unique(prefix0(datalong[,altvarLong],2))[order(unique(prefix0(datalong[,altvarLong],2)))])
    showvars <- c(showvars, dummiesvars)
    datalong[datalong$type=="w",dummiesvars] <- (-1)*datalong[datalong$type=="w",dummiesvars]
  }

  # Order
  cat("\nOrdenando")
  if (isBW) {
    datalong <- datalong[order(datalong[,indvar],datalong$orig.task, datalong[,taskvar],datalong[,"type"],datalong[,altvarLong]),showvars]
  } else {
    datalong <- datalong[order(datalong[,indvar],datalong[,taskvar],datalong[,altvarLong]),showvars]
  }
  rownames(datalong) <- NULL

  if (is.multiplechoice){
    cat("\nExpandiendo compra multiple\n")
    datalong <- dcm_multiple2singlechoice(datalong, choicevar=choicevarLong, chidvar=chidvarLong,
                                          taskvar=taskvar, indvar=indvar)
  }
  datalong
}

# Test
#
# dw1 <- read.delim("~/Consultoria/BleachGel/sintdata/DCM datasint BGel data.txt")
# dl1 <- dcm_wide2long(datawide = datawide,
#                    nalt=29,
#                    indvar = "Indiv", blockvar = NA, taskvar = "Task",
#                    altvarLong="alt", chidvarLong = "chid",
#                    viewPrefix="view", viewvarLong="view", onlyviewdata=T,
#                    pricePrefix="pxvar", pricevarLong="precio", priceType=c("price", "mult", "var"), pxBase,
#                    choicePrefix="choice", choicevarLong="choice",
#                    isBW=F, Bvar, Wvar,
#                    genDummies=T, sep="")
# head(dl1,30)


# DE <- read.delim("//192.168.1.1/sharedR/Consultoria/Max Diff/DE.txt")
# data <- read.delim("//192.168.1.1/sharedR/Consultoria/Max Diff/dataset_test.txt")
# data_full <- merge(x=data, y=DE, by=c("bloque","tarea"), all.x=T)
# data_full <- select(data_full, ind, bloque, tarea, best, worst, starts_with("Obj"))
# data_full <- arrange(data_full, ind, bloque, tarea)
# datalong <- dcm_wide2long(data_full, nalt=49,
#                           indvar="ind", altvar="alt", blockvar="bloque", taskvar="tarea",
#                           chidvarLong = "chid",
#                           viewPrefix="Obj", viewvarLong="view", onlyviewdata=T,
#                           isBW=T, Bvar="best", Wvar="worst",
#                           sep="")
