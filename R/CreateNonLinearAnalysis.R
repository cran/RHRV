CreateNonLinearAnalysis <-
  function(HRVData, verbose=NULL) {
    # ----------------------------------------------------------
    # Creates a non linear analysis associated to the data model
    # ----------------------------------------------------------
    HRVData = HandleVerboseArgument(HRVData, verbose)
    VerboseMessage(HRVData$Verbose, "Creating non linear analysis")
    
    num = length(HRVData$NonLinearAnalysis)
    HRVData$NonLinearAnalysis[[num + 1]] = list()
    
    VerboseMessage(HRVData$Verbose, paste("Data has now ",num + 1," nonlinear analysis"))
    
    return(HRVData)
  }

