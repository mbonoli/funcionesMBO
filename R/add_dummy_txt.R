add_dummy_txt <- function(df, new.var, txt.var, txt){
  # Revisa la variable txt.var en el data.frame df y genera una variable new.var 1/0 si encuentra o no el texto
  df[[new.var]] <- 0
  df[[new.var]][grep(txt, df[[txt.var]])] <- 1
  print (paste0("Dummy generada: ", sum(df[[new.var]])," casos (",100*round(mean(df[[new.var]]),1)," %)"))
  df
}
