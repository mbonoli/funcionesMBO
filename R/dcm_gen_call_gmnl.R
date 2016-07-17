dcm_gen_call_gmnl <- function(choicevar="choice", nalt,
                              dummiesACS=T, refASC, randASC=T, prefixASC="B",
                              adicVar=NA, randAdicVar,
                              data = "datalogit", model, R,
                              haltons = NULL, panel, print.level = 2, method = "bfgs"){

  callgmnl <- "gmnl(formula = "
  callgmnl <- paste0(callgmnl, choicevar, " ~ ")

  # Formula ASC
  if (dummiesACS){
    alts <- 1:nalt
    alts <- alts[-refASC]
    callgmnl <- paste0(callgmnl,
                       gen_seq(prefix="",txt=prefixASC,seqnum=alts,sep=" + ", sufflix=""))
  }
  # Formula Adicionales
  if (!is.na(adicVar[1])){
    callgmnl <- paste0(callgmnl,
                       paste0(" + ", adicVar, collapse = ""))
  }
  callgmnl <- paste0(callgmnl, " | 0, " )
  if (randASC){
    callgmnl <- paste0(callgmnl,
                       gen_seq(prefix=" ranp = c(",txt=prefixASC,seqnum=alts,sep=" = 'n', ", sufflix=" = 'n'), "))
  }
  callgmnl <- paste0(callgmnl, "data = ", data, ", ", collapse = "" )
  callgmnl <- paste0(callgmnl, "model = '", model, "', ", collapse = "" )
  callgmnl <- paste0(callgmnl, "R = ", R, ", ", collapse = "" )
  callgmnl <- paste0(callgmnl, "haltons = '", ifelse(is.null(haltons),"NULL",haltons), "', ", collapse = "" )
  callgmnl <- paste0(callgmnl, "panel = ", panel, ", ", collapse = "" )
  callgmnl <- paste0(callgmnl, "print.level = ", print.level, ", ", collapse = "" )
  callgmnl <- paste0(callgmnl, "method = '", method, "') ", collapse = "" )

  callgmnl
}
