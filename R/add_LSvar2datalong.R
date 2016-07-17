add_LSvar2datalong <- function(datalong, idLS, langLS, vars){
  dls <- ls_get_responses(iSurveyID = idLS, sLanguageCode = langLS,
                          sCompletionStatus = 'all', sResponseType = 'long')
  data2add <- dls[,c("id", vars)]
  result <- merge(x=datalong, y=data2add, by.x="ind", by.y="id", all.x=T)
  result
}
