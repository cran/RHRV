CreateFreqAnalysis <-
function(HRVData, verbose=NULL) {
# ---------------------------------------------------------
# Creates a frequency analysis associated to the data model
# ---------------------------------------------------------

  HRVData = HandleVerboseArgument(HRVData, verbose)
  VerboseMessage(HRVData$Verbose, "Creating frequency analysis")
  num = length(HRVData$FreqAnalysis)
  HRVData$FreqAnalysis[[num + 1]] = list()
  VerboseMessage(HRVData$Verbose, 
                 paste("Data has now", num + 1, "frequency analysis"))
  return(HRVData)
}

