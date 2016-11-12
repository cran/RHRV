AnalyzeHRbyEpisodes <-
function(HRVData, Tag="", func, ..., verbose=NULL) {
# ----------------------------------------------
# Analyzes Heart Rate using Episodes information
# ----------------------------------------------
#  Tag -> specifies tag of episodes
#  func -> function to apply 
#  Returns a list with two objects result

#  Function func musts receive a vector and returns an object

  funcToApply =  match.fun(func)
  nameFunc = deparse(substitute(func))

  HRVData = HandleVerboseArgument(HRVData, verbose)
  VerboseMessage(HRVData$Verbose, 
	paste("Applying function to heart rate signal using episodic information"))
  VerboseMessage(HRVData$Verbose, paste0("Function: ", nameFunc))
   
  CheckEpisodes(HRVData)
  CheckInterpolation(HRVData)
  VerboseMessage(HRVData$Verbose, 
                 ifelse(Tag == "", "No tag was specified",
                        paste("Using episodes with tag:", Tag))
  )
  
  vectors = SplitHRbyEpisodes(HRVData, Tag = Tag)
  result = list(resultIn = funcToApply(vectors$InEpisodes, ...),
                resultOut = funcToApply(vectors$OutEpisodes, ...))
  return(result)
}

