`CreateFreqAnalysis` <-
function(HRVData,verbose=FALSE) {
# ---------------------------------------------------------
# Creates a frequency analysis associated to the data model
# ---------------------------------------------------------
#	Verbose -> TRUE for verbose mode

	if (verbose) {
		cat("** Creating frequency analysis\n")
	}

   num=length(HRVData$FreqAnalysis)

   HRVData$FreqAnalysis[[num+1]]=list()

	if (verbose) {
		cat("   Data has now",num+1,"frequency analysis\n")
	}



   return (HRVData)

}

