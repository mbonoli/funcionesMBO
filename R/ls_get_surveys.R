ls_get_surveys <- function(){

  # http://api.limesurvey.org/classes/remotecontrol_handle.html
  # string 	$sSessionKey 	Auth credentials
  # int 	  $iSurveyID 	Id of the Survey
  # string 	$sDocumentType 	pdf,csv,xls,doc,json
  # string 	$sLanguageCode 	The language to be used
  # string 	$sCompletionStatus 	Optional 'complete','incomplete' or 'all' - defaults to 'all'
  # string 	$sHeadingType 	'code','full' or 'abbreviated' Optional defaults to 'code'
  # string 	$sResponseType 	'short' or 'long' Optional defaults to 'short'
  # integer $iFromResponseID 	Optional
  # integer $iToResponseID 	Optional
  # array 	$aFields 	Optional Selected fields

  require(limer)
  options(lime_api = 'http://www.systemsurvey.com.ar/surveytech/index.php/admin/remotecontrol')
  options(lime_username = 'admin')
  options(lime_password = 'monoli71')

  get_session_key()

  survey_df<-call_limer(method='list_surveys')

  survey_df[survey_df$active=='Y', c(1,2)]
}

