ls_get_responses <- function(iSurveyID, sLanguageCode='es',
                             sResponseType='short', sHeadingType='code',
                             sCompletionStatus='complete'){

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

  data <- get_responses(iSurveyID=iSurveyID, sLanguageCode=sLanguageCode,
                        sResponseType=sResponseType,
                        sHeadingType=sHeadingType, sCompletionStatus=sCompletionStatus)

  data
}
